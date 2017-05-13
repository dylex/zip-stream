module Codec.Archive.Zip.Conduit.Internal
  ( zipVersion
  , zipError
  , idConduit
  , sizeCRC
  , sizeC
  , maxBound32
  , deflateWindowBits
  ) where

import           Codec.Compression.Zlib.Raw (WindowBits(..))
import           Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import           Data.Digest.CRC32 (crc32Update)
import           Data.Word (Word16, Word32, Word64)

import           Codec.Archive.Zip.Conduit.Types

-- |The version of this zip program, really just rough indicator of compatibility
zipVersion :: Word16
zipVersion = 48

zipError :: MonadThrow m => String -> m a
zipError = throwM . ZipError

idConduit :: Monad m => C.Conduit a m a
idConduit = C.awaitForever C.yield

passthroughFold :: Monad m => (a -> b -> a) -> a -> C.ConduitM b b m a
passthroughFold f z = C.await >>= maybe
  (return z)
  (\x -> do
    C.yield x
    passthroughFold f (f z x))

sizeCRC :: Monad m => C.ConduitM BS.ByteString BS.ByteString m (Word64, Word32)
sizeCRC = passthroughFold (\(l, c) b -> (l + fromIntegral (BS.length b), crc32Update c b)) (0, 0)

sizeC :: Monad m => C.ConduitM BS.ByteString BS.ByteString m Word64
sizeC = passthroughFold (\l b -> l + fromIntegral (BS.length b)) 0 -- fst <$> sizeCRC

maxBound32 :: Integral n => n
maxBound32 = fromIntegral (maxBound :: Word32)

deflateWindowBits :: WindowBits
deflateWindowBits = WindowBits (-15)
