-- |Stream the extraction of a zip file, e.g., as it's being downloaded.
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Codec.Archive.Zip.Conduit.UnZip
  ( unZipStream
  , ZipEntry(..)
  , ZipInfo(..)
  ) where

import           Control.Applicative ((<|>), empty)
import           Control.Monad (when, unless, guard)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Catch (MonadThrow(..))
import           Control.Monad.Primitive (PrimMonad)
import qualified Data.Binary.Get as G
import           Data.Bits ((.&.), testBit, clearBit, shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Conduit.Serialization.Binary (sinkGet)
import qualified Data.Conduit.Zlib as CZ
import           Data.Time (LocalTime(..), TimeOfDay(..), fromGregorian)
import           Data.Word (Word16, Word32, Word64)
import           Data.Encoding (decodeStrictByteStringExplicit)
import           Data.Encoding.CP437
import           Data.Encoding.UTF8

import           Codec.Archive.Zip.Conduit.Types
import           Codec.Archive.Zip.Conduit.Internal

data Header m
  = FileHeader
    { fileDecompress :: C.Conduit BS.ByteString m BS.ByteString
    , fileHeaderName :: !BS.ByteString
    , fileNameIsUnicode :: !Bool
    , fileHeaderTime :: !LocalTime
    , fileHeaderEntrySize :: Maybe Word64
    , fileCRC :: !Word32
    , fileCSize :: !Word64
    , fileZip64 :: !Bool
    }
  | EndOfCentralDirectory
    { endInfo :: ZipInfo
    }

data ExtField = ExtField
  { extZip64 :: Bool
  , extZip64USize
  , extZip64CSize :: Word64
  }

{- ExtUnix
  { extUnixATime
  , extUnixMTime :: UTCTime
  , extUnixUID
  , extUnixGID :: Word16
  , extUnixData :: BS.ByteString
  }
-}

pass :: (MonadThrow m, Integral n) => n -> C.Conduit BS.ByteString m BS.ByteString
pass 0 = return ()
pass n = C.await >>= maybe
  (zipError $ "EOF in file data, expecting " ++ show ni ++ " more bytes")
  (\b ->
    let n' = ni - toInteger (BS.length b) in
    if n' < 0
      then do
        let (b', r) = BS.splitAt (fromIntegral n) b
        C.yield b'
        C.leftover r
      else do
        C.yield b
        pass n')
  where ni = toInteger n

foldGet :: (a -> G.Get a) -> a -> G.Get a
foldGet g z = do
  e <- G.isEmpty
  if e then return z else g z >>= foldGet g

fromDOSTime :: Word16 -> Word16 -> LocalTime
fromDOSTime time date = LocalTime
  (fromGregorian
    (fromIntegral $ date `shiftR` 9 + 1980)
    (fromIntegral $ date `shiftR` 5 .&. 0x0f)
    (fromIntegral $ date            .&. 0x1f))
  (TimeOfDay
    (fromIntegral $ time `shiftR` 11)
    (fromIntegral $ time `shiftR` 5 .&. 0x3f)
    (fromIntegral $ time `shiftL` 1 .&. 0x3f))

-- |Stream process a zip file, producing a sequence of entry headers and data blocks.
-- For example, this might produce: @Left (ZipEntry "directory\/" ...), Left (ZipEntry "directory\/file.txt" ...), Right "hello w", Right "orld!\\n", Left ...@
-- The final result is summary information taken from the end of the zip file.
-- No state is maintained during processing, and, in particular, any information in the central directory is discarded.
--
-- This only supports a limited number of zip file features, including deflate compression and zip64.
-- It does not (ironically) support uncompressed zip files that have been created as streams, where file sizes are not known beforehand.
-- Since it does not use the offset information at the end of the file, it assumes all entries are packed sequentially, which is usually the case.
-- Any errors are thrown in the underlying monad (as 'ZipError's or 'Data.Conduit.Serialization.Binary.ParseError').
unZipStream :: (MonadBase b m, PrimMonad b, MonadThrow m) => C.ConduitM BS.ByteString (Either ZipEntry BS.ByteString) m ZipInfo
unZipStream = next where
  next = do -- local header, or start central directory
    h <- sinkGet $ do
      sig <- G.getWord32le
      case sig of
        0x04034b50 -> fileHeader
        _ -> centralBody sig
    case h of
      FileHeader{..} -> do
        name <- either throwM return $ case fileNameIsUnicode of
          True  -> decodeStrictByteStringExplicit UTF8 fileHeaderName
          False -> decodeStrictByteStringExplicit CP437 fileHeaderName
        C.yield . Left $ ZipEntry
          { zipEntryName = name
          , zipEntrySize = fileHeaderEntrySize
          , zipEntryTime = fileHeaderTime
          }
        r <- C.mapOutput Right $
          case fileHeaderEntrySize of
            Nothing -> do -- unknown size
              (csize, (size, crc)) <- inputSize fileDecompress `C.fuseBoth` sizeCRC
              -- traceM $ "csize=" ++ show csize ++ " size=" ++ show size ++ " crc=" ++ show crc
              -- required data description
              sinkGet $ dataDesc h
                { fileCSize = csize
                , fileCRC = crc
                , fileHeaderEntrySize = Just size
                }
            Just usize -> do -- known size
              (size, crc) <- pass fileCSize
                C..| (fileDecompress >> CL.sinkNull)
                C..| sizeCRC
              -- traceM $ "size=" ++ show size ++ "," ++ show (zipEntrySize fileEntry) ++ " crc=" ++ show crc ++ "," ++ show fileCRC
              -- optional data description (possibly ambiguous!)
              sinkGet $ (guard =<< dataDesc h) <|> return ()
              return (size == usize && crc == fileCRC)
        unless r $ zipError $ name ++ ": data integrity check failed"
        next
      EndOfCentralDirectory{..} -> do
        return endInfo
  dataDesc h = -- this takes a bit of flexibility to account for the various cases
    (do -- with signature
      sig <- G.getWord32le
      guard (sig == 0x08074b50)
      dataDescBody h)
    <|> dataDescBody h -- without signature
  dataDescBody FileHeader{..} = do
    crc <- G.getWord32le
    let getSize = if fileZip64 then G.getWord64le else fromIntegral <$> G.getWord32le
    csiz <- getSize
    usiz <- getSize
    -- traceM $ "crc=" ++ show crc ++ "," ++ show fileCRC ++ " csiz=" ++ show csiz ++ "," ++ show fileCSize ++ " usiz=" ++ show usiz ++ "," ++ show (zipEntrySize fileEntry)
    return $ crc == fileCRC && csiz == fileCSize && (usiz ==) `all` fileHeaderEntrySize
  dataDescBody _ = empty
  central = G.getWord32le >>= centralBody
  centralBody 0x02014b50 = centralHeader >> central
  centralBody 0x06064b50 = zip64EndDirectory >> central
  centralBody 0x07064b50 = G.skip 16 >> central
  centralBody 0x06054b50 = EndOfCentralDirectory <$> endDirectory
  centralBody sig = fail $ "Unknown header signature: " ++ show sig
  fileHeader = do
    ver <- G.getWord16le
    when (ver > zipVersion) $ fail $ "Unsupported version: " ++ show ver
    gpf <- G.getWord16le
    -- when (gpf .&. complement (bit 1 .|. bit 2 .|. bit 3) /= 0) $ fail $ "Unsupported flags: " ++ show gpf
    when (gpf `clearBit` 1 `clearBit` 2 `clearBit` 3 `clearBit` 11 /= 0) $ fail $ "Unsupported flags: " ++ show gpf
    comp <- G.getWord16le
    dcomp <- case comp of
      0 | testBit gpf 3 -> fail "Unsupported uncompressed streaming file data"
        | otherwise -> return idConduit
      8 -> return $ CZ.decompress deflateWindowBits
      _ -> fail $ "Unsupported compression method: " ++ show comp
    time <- fromDOSTime <$> G.getWord16le <*> G.getWord16le
    crc <- G.getWord32le
    csiz <- G.getWord32le
    usiz <- G.getWord32le
    nlen <- fromIntegral <$> G.getWord16le
    elen <- fromIntegral <$> G.getWord16le
    name <- G.getByteString nlen
    let getExt ext = do
          t <- G.getWord16le
          z <- fromIntegral <$> G.getWord16le
          G.isolate z $ case t of
            0x0001 -> do
              -- the zip specs claim "the Local header MUST include BOTH" but "only if the corresponding field is set to 0xFFFFFFFF"
              usiz' <- if usiz == maxBound32 then G.getWord64le else return $ extZip64USize ext
              csiz' <- if csiz == maxBound32 then G.getWord64le else return $ extZip64CSize ext
              return ext
                { extZip64 = True
                , extZip64USize = usiz'
                , extZip64CSize = csiz'
                }
            {-
            0x000d -> do
              atim <- G.getWord32le
              mtim <- G.getWord32le
              uid <- G.getWord16le
              gid <- G.getWord16le
              dat <- G.getByteString $ z - 12
              return ExtUnix
                { extUnixATime = posixSecondsToUTCTime atim
                , extUnixMTime = posixSecondsToUTCTime mtim
                , extUnixUID = uid
                , extUnixGID = gid
                , extUnixData = dat
                }
            -}
            _ -> ext <$ G.skip z
    ExtField{..} <- G.isolate elen $ foldGet getExt ExtField
      { extZip64 = False
      , extZip64USize = fromIntegral usiz
      , extZip64CSize = fromIntegral csiz
      }
    return FileHeader
      { fileHeaderName = name
      , fileNameIsUnicode = testBit gpf 11
      , fileHeaderTime = time
      , fileHeaderEntrySize = if testBit gpf 3 then Nothing else Just extZip64USize
      , fileDecompress = dcomp
      , fileCSize = extZip64CSize
      , fileCRC = crc
      , fileZip64 = extZip64
      }
  centralHeader = do
    -- ignore everything
    G.skip 24
    nlen <- fromIntegral <$> G.getWord16le
    elen <- fromIntegral <$> G.getWord16le
    clen <- fromIntegral <$> G.getWord16le
    G.skip $ 12 + nlen + elen + clen
  zip64EndDirectory = do
    len <- G.getWord64le
    G.skip $ fromIntegral len -- would not expect to overflow...
  endDirectory = do
    G.skip 16
    clen <- fromIntegral <$> G.getWord16le
    comm <- G.getByteString clen
    return ZipInfo
      { zipComment = comm
      }
