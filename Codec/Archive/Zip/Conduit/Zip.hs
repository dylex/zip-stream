{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Codec.Archive.Zip.Conduit.Zip
  ( ZipOptions(..)
  , defaultZipOptions
  , ZipEntry(..)
  , ZipData(..)
  , zipFileData
  , zipStream
  ) where

import qualified Codec.Compression.Zlib.Raw as Z
import           Control.Arrow ((&&&), (+++), left)
import           Control.Monad (when)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Binary.Put as P
import           Data.Bits (bit, shiftL, shiftR, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Serialization.Binary (sourcePut)
import           Data.Conduit.Zlib (compress)
import           Data.Digest.CRC32 (crc32)
import           Data.Either (isLeft)
import           Data.Maybe (fromMaybe)
import           Data.Time (LocalTime(..), TimeOfDay(..), toGregorian)
import           Data.Word (Word16, Word64)

import           Codec.Archive.Zip.Conduit.Types
import           Codec.Archive.Zip.Conduit.Internal

data ZipOptions = ZipOptions
  { zipOpt64 :: Bool -- ^Allow zip file sizes over 4GB (reduces compatibility, but is otherwise safe for any file sizes)
  , zipOptCompressLevel :: Int -- ^Compress (0 = store only, 9 = best) zipped files (improves compatibility, since some unzip programs don't supported stored, streamed files, including the one in this package)
  , zipOptInfo :: ZipInfo -- ^Other parameters to store in the zip file
  }

defaultZipOptions :: ZipOptions
defaultZipOptions = ZipOptions
  { zipOpt64 = False
  , zipOptCompressLevel = -1
  , zipOptInfo = ZipInfo
    { zipComment = BS.empty
    }
  }

data ZipData m
  = ZipDataByteString BSL.ByteString
  | ZipDataSource (C.Source m BS.ByteString)

instance Monad m => Monoid (ZipData m) where
  mempty = ZipDataByteString BSL.empty
  mappend (ZipDataByteString a) (ZipDataByteString b) = ZipDataByteString $ mappend a b
  mappend a b = ZipDataSource $ mappend (zipDataSource a) (zipDataSource b)

zipFileData :: MonadResource m => FilePath -> ZipData m
zipFileData = ZipDataSource . CB.sourceFile

zipDataSource :: Monad m => ZipData m -> C.Source m BS.ByteString
zipDataSource (ZipDataByteString b) = CB.sourceLbs b
zipDataSource (ZipDataSource s) = s

zipData :: Monad m => ZipData m -> Either (C.Source m BS.ByteString) BSL.ByteString
zipData (ZipDataByteString b) = Right b
zipData (ZipDataSource s) = Left s

dataSize :: Either a BSL.ByteString -> Maybe Word64
dataSize (Left _) = Nothing
dataSize (Right b) = Just $ fromIntegral $ BSL.length b

toDOSTime :: LocalTime -> (Word16, Word16)
toDOSTime (LocalTime (toGregorian -> (year, month, day)) (TimeOfDay hour mins secs)) = 
  ( fromIntegral hour `shiftL` 11 .|. fromIntegral mins `shiftL` 5 .|. truncate secs `shiftR` 1
  , fromIntegral (year - 1980) `shiftL` 9 .|. fromIntegral month `shiftL` 5 .|. fromIntegral day
  )

zipStream :: (MonadBase b m, PrimMonad b, MonadThrow m) => ZipOptions -> C.ConduitM (ZipEntry, ZipData m) BS.ByteString m Word64
zipStream ZipOptions{..} = do
  C.awaitForever $ C.toProducer . entry
  return 0 -- TODO: size
  where
  entry (ZipEntry{..}, zipData -> dat) = do
    let usiz = dataSize dat
        sdat = left (C..| sizeCRC) dat
        comp = zipOptCompressLevel /= 0 && all (0 /=) usiz
        (cdat, csiz)
          | comp =
            ( ((`C.fuseBoth` (compress zipOptCompressLevel deflateWindowBits C..| (fst <$> sizeCRC)))
              +++ Z.compress) sdat -- level for Z.compress?
            , dataSize cdat)
          | otherwise = (left (fmap (id &&& fst)) sdat, usiz)
        z64 = maybe zipOpt64 (zip64Size <) (max <$> usiz <*> csiz)
        namelen = BS.length zipEntryName
    when (namelen > fromIntegral (maxBound :: Word16)) $ zipError $ BSC.unpack zipEntryName ++ ": entry name too long"
    sourcePut $ do
      P.putWord32le 0x04034b50
      P.putWord16le $ if z64 then 45 else 20
      P.putWord16le $ if isLeft dat then bit 3 else 0
      P.putWord16le $ if comp then 8 else 0
      let (time, date) = toDOSTime zipEntryTime
      P.putWord16le $ time
      P.putWord16le $ date
      P.putWord32le $ either (const 0) crc32 cdat
      P.putWord32le $ if z64 then zip64Size else maybe 0 fromIntegral csiz
      P.putWord32le $ if z64 then zip64Size else maybe 0 fromIntegral usiz
      P.putWord16le $ fromIntegral namelen
      P.putWord16le $ if z64 then 20 else 0
      P.putByteString zipEntryName
      when z64 $ do
        P.putWord16le 0x0001
        P.putWord16le 16
        P.putWord64le $ fromMaybe 0 usiz
        P.putWord64le $ fromMaybe 0 csiz
    either
      (\cd -> do
        ((usz, crc), csz) <- cd -- write compressed data
        when (not z64 && (usz > zip64Size || csz > zip64Size)) $ zipError $ BSC.unpack zipEntryName ++ ": file too large and zipOpt64 disabled"
        sourcePut $ do
          P.putWord32le 0x08074b50
          P.putWord32le crc
          let putsz
                | z64 = P.putWord64le
                | otherwise = P.putWord32le . fromIntegral
          putsz csz
          putsz usz)
      CB.sourceLbs
      cdat
