{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import           Data.Time.LocalTime (localTimeToUTC, utc)
import           System.Directory (createDirectoryIfMissing
#if MIN_VERSION_directory(1,2,3)
  , setModificationTime
#endif
  )
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.FilePath.Posix (takeDirectory) -- zip files only use forward slashes
import           System.IO (stdin, openFile, IOMode(WriteMode), hClose, hSetFileSize, hPutStrLn, stderr)

import           Codec.Archive.Zip.Conduit.UnZip

extract :: C.Sink (Either ZipEntry BS.ByteString) IO ()
extract = C.awaitForever start where
  start (Left ZipEntry{..}) = do
    liftIO $ putStrLn zipEntryName
    liftIO $ createDirectoryIfMissing True (takeDirectory name)
    if last zipEntryName == '/'
      then when ((0 /=) `any` zipEntrySize) $ fail $ name ++ ": non-empty directory"
      else do -- C.bracketP
        h <- liftIO $ openFile name WriteMode
        mapM_ (liftIO . hSetFileSize h . toInteger) zipEntrySize
        write C..| CB.sinkHandle h
        liftIO $ hClose h
#if MIN_VERSION_directory(1,2,3)
    liftIO $ setModificationTime name $ localTimeToUTC utc zipEntryTime -- FIXME: timezone
#endif
    where name = dropWhile ('/' ==) zipEntryName -- should we utf8 decode?
  start (Right _) = fail "Unexpected leading or directory data contents"
  write = C.await >>= maybe
    (return ())
    block
  block (Right b) = C.yield b >> write
  block a = C.leftover a

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  unless (null args) $ do
    hPutStrLn stderr $ "Usage: " ++ prog ++ "\nRead a zip file from stdin and extract it in the current directory."
    exitFailure
  ZipInfo{..} <- C.runConduit 
    $ CB.sourceHandle stdin
    C..| C.fuseUpstream unZipStream extract
  BSC.putStrLn zipComment
