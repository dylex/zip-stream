{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Codec.Archive.Zip.Conduit.Zip
  ( ZipOptions(..)
  , ZipInfo(..)
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
import           Control.Monad.State.Strict (StateT, get)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Binary.Put as P
import           Data.Bits (bit, shiftL, shiftR, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Lift (stateC, execStateC)
import           Data.Conduit.Serialization.Binary (sourcePut)
import qualified Data.Conduit.Zlib as CZ
import           Data.Digest.CRC32 (crc32)
import           Data.Either (isLeft)
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Monoid ((<>))
import           Data.Time (LocalTime(..), TimeOfDay(..), toGregorian)
import           Data.Word (Word16, Word64)

import           Codec.Archive.Zip.Conduit.Types
import           Codec.Archive.Zip.Conduit.Internal

data ZipOptions = ZipOptions
  { zipOpt64 :: Bool -- ^Allow 'ZipDataSource's over 4GB (reduces compatibility in some cases)
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

infixr 7 ?*
(?*) :: Num a => Bool -> a -> a
True ?* x = x
False ?* _ = 0

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

countBytes :: Monad m => C.ConduitM i BS.ByteString m a -> C.ConduitM i BS.ByteString (StateT Word64 m) a
countBytes c = stateC $ \s -> c `C.fuseBoth` ((s +) <$> sizeC)

output :: MonadThrow m => P.Put -> C.ConduitM i BS.ByteString (StateT Word64 m) ()
output = countBytes . sourcePut

maxBound16 :: Integral n => n
maxBound16 = fromIntegral (maxBound :: Word16)

zipStream :: (MonadBase b m, PrimMonad b, MonadThrow m) => ZipOptions -> C.ConduitM (ZipEntry, ZipData m) BS.ByteString m Word64
zipStream ZipOptions{..} = execStateC 0 $ do
  (cnt, cdir) <- next 0 (mempty :: P.Put)
  cdoff <- get
  output cdir
  eoff <- get
  endDirectory cdoff (eoff - cdoff) cnt 
  where
  next cnt dir = C.await >>= maybe
    (return (cnt, dir))
    (\e -> do
      d <- entry e
      next (succ cnt) $ dir <> d)
  entry (ZipEntry{..}, zipData -> dat) = do
    let usiz = dataSize dat
        sdat = left ((C..| sizeCRC) . C.toProducer) dat
        comp = zipOptCompressLevel /= 0 && all (0 /=) usiz
        (cdat, csiz)
          | comp =
            ( ((`C.fuseBoth` (CZ.compress zipOptCompressLevel deflateWindowBits C..| sizeC))
              +++ Z.compress) sdat -- level for Z.compress?
            , dataSize cdat)
          | otherwise = (left (fmap (id &&& fst)) sdat, usiz)
        z64 = maybe zipOpt64 (maxBound32 <) (max <$> usiz <*> csiz)
        namelen = BS.length zipEntryName
        (time, date) = toDOSTime zipEntryTime
        mcrc = either (const Nothing) (Just . crc32) cdat
    when (namelen > maxBound16) $ zipError $ BSC.unpack zipEntryName ++ ": entry name too long"
    let common = do
          P.putWord16le $ isLeft dat ?* bit 3
          P.putWord16le $ comp ?* 8
          P.putWord16le $ time
          P.putWord16le $ date
    off <- get
    output $ do
      P.putWord32le 0x04034b50
      P.putWord16le $ if z64 then 45 else 20
      common
      P.putWord32le $ fromMaybe 0 mcrc
      P.putWord32le $ if z64 then maxBound32 else maybe 0 fromIntegral csiz
      P.putWord32le $ if z64 then maxBound32 else maybe 0 fromIntegral usiz
      P.putWord16le $ fromIntegral namelen
      P.putWord16le $ z64 ?* 20
      P.putByteString zipEntryName
      when z64 $ do
        P.putWord16le 0x0001
        P.putWord16le 16
        P.putWord64le $ fromMaybe 0 usiz
        P.putWord64le $ fromMaybe 0 csiz
    let outsz c = stateC $ \o -> (id &&& (o +) . snd) <$> c
    ((usz, crc), csz) <- either
      (\cd -> do
        r@((usz, crc), csz) <- outsz cd -- write compressed data
        when (not z64 && (usz > maxBound32 || csz > maxBound32)) $ zipError $ BSC.unpack zipEntryName ++ ": file too large and zipOpt64 disabled"
        output $ do
          P.putWord32le 0x08074b50
          P.putWord32le crc
          let putsz
                | z64 = P.putWord64le
                | otherwise = P.putWord32le . fromIntegral
          putsz csz
          putsz usz
        return r)
      (\b -> outsz $ ((fromJust usiz, fromJust mcrc), fromJust csiz) <$ CB.sourceLbs b)
      cdat
    return $ do
      -- central directory
      let o64 = off >= maxBound32
          l64 = z64 ?* 16 + o64 ?* 8
          a64 = z64 || o64
      P.putWord32le 0x02014b50
      P.putWord16le zipVersion
      P.putWord16le $ if a64 then 45 else 20
      common
      P.putWord32le crc
      P.putWord32le $ if z64 then maxBound32 else fromIntegral csz
      P.putWord32le $ if z64 then maxBound32 else fromIntegral usz
      P.putWord16le $ fromIntegral namelen
      P.putWord16le $ a64 ?* (4 + l64)
      P.putWord16le 0 -- comment length
      P.putWord16le 0 -- disk number
      P.putWord16le 0 -- internal file attributes
      P.putWord32le 0 -- external file attributes
      P.putWord32le $ if o64 then maxBound32 else fromIntegral off
      P.putByteString zipEntryName
      when a64 $ do
        P.putWord16le 0x0001
        P.putWord16le l64
        when z64 $ do
          P.putWord64le usz
          P.putWord64le csz
        when o64 $
          P.putWord64le off
  endDirectory cdoff cdlen cnt = do
    let z64 = zipOpt64 || cdoff > maxBound32 || cnt > maxBound16
    when z64 $ output $ do
      P.putWord32le 0x06064b50 -- zip64 end
      P.putWord64le 44 -- length of this record
      P.putWord16le zipVersion
      P.putWord16le 45
      P.putWord32le 0 -- disk
      P.putWord32le 0 -- central disk
      P.putWord64le cnt
      P.putWord64le cnt
      P.putWord64le cdlen
      P.putWord64le cdoff
      P.putWord32le 0x07064b50 -- locator:
      P.putWord32le 0 -- central disk
      P.putWord64le $ cdoff + cdlen
      P.putWord32le 1 -- total disks
    let comment = zipComment zipOptInfo
        commlen = BS.length comment
    when (commlen > maxBound16) $ zipError "comment too long"
    output $ do
      P.putWord32le 0x06054b50 -- end
      P.putWord16le 0 -- disk
      P.putWord16le 0 -- central disk
      P.putWord16le $ fromIntegral $ min maxBound16 cnt
      P.putWord16le $ fromIntegral $ min maxBound16 cnt
      P.putWord32le $ fromIntegral $ min maxBound32 cdlen
      P.putWord32le $ fromIntegral $ min maxBound32 cdoff
      P.putWord16le $ fromIntegral commlen
      P.putByteString comment
