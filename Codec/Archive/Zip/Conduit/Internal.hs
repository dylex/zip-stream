module Codec.Archive.Zip.Conduit.Internal
  ( zipError
  , sizeCRC
  ) where

import           Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import           Data.Digest.CRC32 (crc32Update)
import           Data.Word (Word32)

import           Codec.Archive.Zip.Conduit.Types

zipError :: MonadThrow m => String -> m a
zipError = throwM . ZipError

passthroughFold :: Monad m => (a -> b -> a) -> a -> C.ConduitM b b m a
passthroughFold f z = C.await >>= maybe
  (return z)
  (\x -> do
    C.yield x
    passthroughFold f (f z x))

sizeCRC :: (Monad m, Integral n) => C.ConduitM BS.ByteString BS.ByteString m (n, Word32)
sizeCRC = passthroughFold (\(l, c) b -> (l + fromIntegral (BS.length b), crc32Update c b)) (0, 0)
