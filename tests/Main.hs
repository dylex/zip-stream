{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad (when, void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C (sinkLbs, sourceLbs)
import           Data.Conduit.Combinators as C -- (sinkFile, sinkNull)
import           Data.Foldable (for_)
import qualified Data.Text as T
import           Data.Time.LocalTime (utc, utcToLocalTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           GHC.Stats (getRTSStats, RTSStats(..), GCDetails(..))
import           System.Mem (performMajorGC)
import           Test.Hspec (describe, hspec, it, shouldBe)

import           Codec.Archive.Zip.Conduit.Zip
import           Codec.Archive.Zip.Conduit.UnZip


main :: IO ()
main = hspec $ do
  describe "zipping" $ do
    it "ZipDataByteString streams in constant memory" $ do
      C.runConduitRes $
        (do
          -- Stream 1000 * 4 MiB = 4 GiB
          for_ [(1::Int)..1024] $ \i -> do
            -- `bs` needs to depend on loop variable `i`, otherwise GHC may hoist
            -- it out of the loop ("floating"), making the memory constant
            -- even for incorrect implementations, thus making the test useless.
            let !bs = BS.replicate (4 * 1024 * 1024) (fromIntegral i) -- 4 MiB

            C.yield
              ( ZipEntry
                  { zipEntryName = Left ("file-" <> T.pack (show i) <> ".bin")
                  , zipEntryTime = utcToLocalTime utc (posixSecondsToUTCTime 0)
                  , zipEntrySize = Nothing
                  , zipEntryExternalAttributes = Nothing
                  }
              , ZipDataByteString (BSL.fromStrict bs) -- `copy` to avoid sharing
              )

            liftIO $ do
              -- GC every 40 MB to make it easy to observe constant memory.
              when (i `mod` 10 == 0) performMajorGC

              RTSStats{ gc = GCDetails{ gcdetails_live_bytes } } <- getRTSStats
              when (gcdetails_live_bytes > 3 * 1024 * 1024 * 1024) $ do -- 3 GiB
                error $ "Memory usage too high (" ++ show gcdetails_live_bytes ++ " B), probably streaming is not constant-memory"

        )
        C..| void (zipStream defaultZipOptions{ zipOptCompressLevel = 0 })
        C..| sinkNull
        :: IO ()

    it "ZipDataSource behaves correctly with empty conduits" $ do
      zipbytes <- C.runConduitRes
                $ entries
               .| void (zipStream defaultZipOptions)
               .| C.sinkLbs
      C.runConduitRes $ C.sourceLbs zipbytes .| C.sinkFile "/tmp/test.zip"
      ZipInfo{..} <- C.runConduitRes
                   $ C.sourceLbs zipbytes
                  .| C.fuseUpstream unZipStream (C.awaitForever assertItem)
      zipComment `shouldBe` ""
    where
      entries = do
        C.yield ( simpleZipEntry "roses.txt"
                , ZipDataSource (C.yield "Roses are red\n")
                )
        C.yield (simpleZipEntry "empty_OK_1.txt", ZipDataByteString "")
        C.yield (simpleZipEntry "empty_OK_2.txt", ZipDataSource emptySingleChunk)
        C.yield (simpleZipEntry "empty_BUG.txt", ZipDataSource emptyNoYield)
        C.yield (simpleZipEntry "trailer.txt", ZipDataByteString "FIN")

      emptySingleChunk = C.yield ""
      emptyNoYield = mempty -- return ()

      posixEpoch = utcToLocalTime utc (posixSecondsToUTCTime 0)
      simpleZipEntry fname = ZipEntry{..} where
        zipEntryName = Left fname
        zipEntryTime = posixEpoch
        zipEntrySize = Nothing
        zipEntryExternalAttributes = Nothing

      assertItem (Right _) = fail "Unexpected leading or directory data contents"
      assertItem (Left ZipEntry{..}) = liftIO $ do
        zipEntryTime `shouldBe` posixEpoch
        when (zipEntryName == Left "roses.txt") $ zipEntrySize `shouldBe` Just 14
        when (zipEntryName == Left "empty_OK_1.txt") $ zipEntrySize `shouldBe` Just 0
        when (zipEntryName == Left "empty_OK_2.txt") $ zipEntrySize `shouldBe` Just 0
        when (zipEntryName == Left "empty_BUG.txt") $ zipEntrySize `shouldBe` Just 0
        when (zipEntryName == Left "trailer.txt") $ zipEntrySize `shouldBe` Just 3

