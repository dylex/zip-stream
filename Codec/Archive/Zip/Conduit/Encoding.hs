{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Archive.Zip.Conduit.Encoding
  ( encodeZipEntryName
  , decodeZipEntryName
  , encodeZipEntry
  ) where

import Codec.Archive.Zip.Conduit.Types (ZipEntry(..))
import Data.Encoding ( decodeStrictByteStringExplicit
                     , encodeStrictByteStringExplicit
                     )
import Data.Encoding.CP437
import Data.Encoding.UTF8

import Data.Time (LocalTime)
import Data.Word (Word64)

import Data.Maybe
import Data.Monoid
import Data.Foldable (asum)
import Control.Monad
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Trans.Except

import Data.ByteString (ByteString)


decodeZipEntryName :: MonadThrow m => ZipEntry -> m FilePath
-- ^ Extract the filename from a 'ZipEntry' doing decoding along the way.
--
-- Throws 'Data.Encoding.Exception.DecodingException's.
decodeZipEntryName ZipEntry{..}
  | zipEntryNameIsUTF8 = either throwM return $ decodeStrictByteStringExplicit UTF8 zipEntryName
  | otherwise          = either throwM return $ decodeStrictByteStringExplicit CP437 zipEntryName
  
encodeZipEntryName :: MonadThrow m => FilePath -> m (Bool, ByteString)
-- ^ Encode a filename for use in a 'ZipEntry', returns a 'Bool' indicating
--   whether the result had to be encoded as 'UTF8' rather than the standard
--   'CP437'.
--
-- Does not do any normalisation (in particular this function does not ensure
-- that the 'FilePath' does not start with a slash).
--
-- Throws 'Data.Encoding.Exception.EncodingException's if the 'FilePath' cannot
-- be encoded as 'UTF8' (which we assume to be a strict superset of 'CP437').
encodeZipEntryName path = either (throwM . fromJust . getLast) return <=< runExceptT $ asum
  [ fmap (False, ) . either (throwE . Last . Just) return $ encodeStrictByteStringExplicit CP437 path
  , fmap (True, ) . either (throwE . Last . Just) return $ encodeStrictByteStringExplicit UTF8 path
  ]

encodeZipEntry :: MonadThrow m
               => FilePath -- ^ 'zipEntryName'
               -> LocalTime -- ^ 'zipEntryTime'
               -> Maybe Word64 -- ^ 'zipEntrySize'
               -> m ZipEntry
-- ^ Smart constructor for 'ZipEntry' which calls 'encodeZipEntryName'.
encodeZipEntry path zipEntryTime zipEntrySize = do
  (zipEntryNameIsUTF8, zipEntryName) <- encodeZipEntryName path
  return ZipEntry{..}
