module Codec.Archive.Zip.Conduit.Types where

import           Control.Exception (Exception(..))
import           Data.ByteString (ByteString)
import           Data.String (IsString(..))
import           Data.Time.LocalTime (LocalTime)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)

-- |Errors thrown during zip file processing
newtype ZipError = ZipError String
  deriving (Show, Typeable)

instance IsString ZipError where
  fromString = ZipError

instance Exception ZipError where
  displayException (ZipError e) = "ZipError: " ++ e

-- |(The beginning of) a single entry in a zip stream, which may be any file or directory.
-- As per zip file conventions, directory names should end with a slash and have no data, but this library does not ensure that.
data ZipEntry = ZipEntry
  { zipEntryName :: ByteString -- ^File name (in posix format, no leading slashes), usually utf-8 encoded, with a trailing slash for directories
  , zipEntryTime :: LocalTime -- ^Modification time
  , zipEntrySize :: Maybe Word64 -- ^Size of file data (if known); checked on zipping and also used as hint to enable zip64
  }

-- |Summary information at the end of a zip stream.
data ZipInfo = ZipInfo
  { zipComment :: ByteString
  }
