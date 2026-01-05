module Servant.Client.Internal.ByteString where

import Foreign
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Miso.DSL

foreign import javascript unsafe "return new Uint8Array(__exports.memory.buffer, $1, $2).slice();"
  createCopy :: Ptr Word8 -> Int -> IO JSVal

sendToJS :: ByteString -> IO JSVal
sendToJS bs = do
  B.useAsCStringLen bs $ \(ptr, len) -> createCopy (castPtr ptr) len

foreign import javascript unsafe "return $1.byteLength;"
  jsByteLength :: JSVal -> IO Int

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer).set($1 instanceof ArrayBuffer ? new Uint8Array($1) : $1, $2);"
  copyToWasm :: JSVal -> Ptr Word8 -> IO ()

getFromJS :: JSVal -> IO ByteString
getFromJS jsVal = do
  len <- jsByteLength jsVal
  B.create len $ copyToWasm jsVal
