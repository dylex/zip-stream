{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Codec.Archive.Zip.Conduit.UnZip
  ( ZipEntry(..)
  , ZipInfo(..)
  , unZip
  ) where

import           Control.Monad (when, unless)
import qualified Data.Binary.Get as G
import           Data.Bits ((.&.), complement, shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Conduit.Serialization.Binary (sinkGet)
import           Data.Conduit.Zlib (WindowBits(..), decompress)
import           Data.Digest.CRC32 (crc32Update)
import           Data.Time (UTCTime(..), fromGregorian, timeOfDayToTime, TimeOfDay(..))
import           Data.Word (Word, Word32)

data ZipEntry = ZipEntry
  { zipEntryName :: BS.ByteString
  , zipEntryTime :: UTCTime
  , zipEntrySize :: Word
  }

data ZipInfo = ZipInfo
  { zipComment :: BS.ByteString
  }

data Header m
  = FileHeader
    { fileDecompress :: C.Conduit BS.ByteString m BS.ByteString
    , fileEntry :: !ZipEntry
    , fileCRC :: !Word32
    , fileCSize :: !Word32
    }
  | EndOfCentralDirectory
    { endInfo :: ZipInfo
    }

crc32 :: Monad m => C.Consumer BS.ByteString m Word32
crc32 = CL.fold crc32Update 0

checkCRC :: Monad m => Word32 -> C.Conduit BS.ByteString m BS.ByteString
checkCRC t = C.passthroughSink crc32 $ \r -> unless (r == t) $ fail "CRC32 mismatch"

unZip :: C.ConduitM BS.ByteString (Either ZipEntry BS.ByteString) IO ZipInfo
unZip = next where
  next = do
    h <- sinkGet header
    case h of
      FileHeader{..} -> do
        C.yield $ Left fileEntry
        C.mapOutput Right $ pass (fromIntegral fileCSize)
          C..| (fileDecompress >> CL.sinkNull)
          C..| checkCRC fileCRC
        next
      EndOfCentralDirectory{..} -> do
        return endInfo
  header = do
    sig <- G.getWord32le
    case sig of
      0x04034b50 -> fileHeader
      0x08074b50 -> -- data descriptor
        G.skip 12 >> header
      _ -> centralDirectory sig
  centralDirectory 0x02014b50 = centralHeader >> G.getWord32le >>= centralDirectory
  centralDirectory 0x06054b50 = EndOfCentralDirectory <$> endDirectory
  centralDirectory sig = fail $ "Unknown header signature: " ++ show sig
  fileHeader = do
    ver <- G.getWord16le
    when (ver > 20) $ fail $ "Unsupported version: " ++ show ver
    gpf <- G.getWord16le
    when (gpf .&. complement 6 /= 0) $ fail $ "Unsupported flags: " ++ show gpf
    comp <- G.getWord16le
    dcomp <- case comp of
      0 -> return $ C.awaitForever C.yield
      8 -> return $ decompress (WindowBits (-15))
      _ -> fail $ "Unsupported compression method: " ++ show comp
    time <- G.getWord16le
    date <- G.getWord16le
    let mtime = UTCTime (fromGregorian
            (fromIntegral $ date `shiftR` 9 + 1980)
            (fromIntegral $ date `shiftR` 5 .&. 0x0f)
            (fromIntegral $ date            .&. 0x1f)
          )
          (timeOfDayToTime $ TimeOfDay
            (fromIntegral $ time `shiftR` 11)
            (fromIntegral $ time `shiftR` 5 .&. 0x3f)
            (fromIntegral $ time `shiftL` 1 .&. 0x3f)
          )
    crc <- G.getWord32le
    csiz <- G.getWord32le
    usiz <- G.getWord32le
    nlen <- G.getWord16le
    elen <- G.getWord16le
    name <- G.getByteString $ fromIntegral nlen
    G.skip $ fromIntegral elen
    return FileHeader
      { fileEntry = ZipEntry
        { zipEntryName = name
        , zipEntryTime = mtime
        , zipEntrySize = fromIntegral usiz
        }
      , fileDecompress = dcomp
      , fileCSize = csiz
      , fileCRC = crc
      }
  centralHeader = do
    -- ignore everything
    G.skip 24
    nlen <- G.getWord16le
    elen <- G.getWord16le
    clen <- G.getWord16le
    G.skip $ 12 + fromIntegral nlen + fromIntegral elen + fromIntegral clen
  endDirectory = do
    G.skip 16
    clen <- G.getWord16le
    comm <- G.getByteString $ fromIntegral clen
    return ZipInfo
      { zipComment = comm
      }
  pass 0 = return ()
  pass n = C.await >>= maybe
    (fail $ "EOF in file data, expecting " ++ show n ++ " more bytes")
    (\b -> do
      let (b', r) = BS.splitAt n b
      C.yield b'
      if BS.null r
        then pass $ n - BS.length b'
        else C.leftover r)
