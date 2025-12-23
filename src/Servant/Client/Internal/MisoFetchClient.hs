{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.Client.Internal.MisoFetchClient where

import           Prelude ()
import           Prelude.Compat

import           Control.Concurrent
                 (newEmptyMVar, takeMVar, putMVar)
import           Control.Exception
                 (Exception)
import           Control.Monad.Catch
                 (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class
                 (MonadError (..))
import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.Reader
                 (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.Trans.Except
                 (ExceptT, runExceptT)
import           Data.Bifunctor
                 (bimap)
import           Data.ByteString.Builder
                 (toLazyByteString)
import qualified Data.ByteString.Char8             as BS
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.ByteString.Lazy              as L
import           Data.CaseInsensitive
                 (mk, original)
import           Data.Foldable
                 (toList)
import           Data.Functor.Alt
                 (Alt (..))
import qualified Data.Map as Map
import           Data.Maybe
                 (fromMaybe)
import           Data.Proxy
                 (Proxy (..))
import qualified Data.Sequence                     as Seq
import qualified Data.Text.Encoding                as T
import           GHC.Generics
import qualified GHCJS.Buffer                      as Buffer
import           GHCJS.DOM.Types
                 (JSM, JSContextRef, askJSM, runJSM)
import qualified GHCJS.DOM.Types                   as JS
import qualified JavaScript.TypedArray.ArrayBuffer as ArrayBuffer
import qualified Language.Javascript.JSaddle.Types as JSaddle
import           Network.HTTP.Media
                 (renderHeader)
import           Network.HTTP.Types
                 (Status, http11, mkStatus, renderQuery)

import           Servant.Client.Core
import qualified Miso as Miso

-- Note: assuming encoding UTF-8

data ClientEnv
   = ClientEnv
   { baseUrl :: BaseUrl
   }

data JSaddleConnectionError = JSaddleConnectionError
  deriving (Eq, Show)

instance Exception JSaddleConnectionError

-- | Default 'ClientEnv'
mkClientEnv :: BaseUrl -> ClientEnv
mkClientEnv burl = ClientEnv burl

instance Show ClientEnv where
  showsPrec prec (ClientEnv burl) =
    showParen (prec >= 11)
      ( showString "ClientEnv {"
      . showString "baseUrl = "
      . showsPrec 0 burl
      . showString "}"
      )

client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

newtype ClientM a = ClientM
  { fromClientM :: ReaderT ClientEnv (ExceptT ClientError JSM) a }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader ClientEnv, MonadError ClientError)
deriving instance MonadThrow JSM => MonadThrow ClientM
deriving instance MonadCatch JSM => MonadCatch ClientM

-- | Try clients in order, last error is preserved.
instance Alt ClientM where
  a <!> b = a `catchError` const b

instance RunClient ClientM where
  throwClientError = throwError
#if MIN_VERSION_servant_client_core(0,18,1)
  runRequestAcceptStatus acceptStatuses r = do
    d <- ClientM askJSM
    performRequest (fromMaybe [] acceptStatuses) d r
#else
  runRequest r = do
    d <- ClientM askJSM
    performRequest [] d r
#endif

runClientM :: ClientM a -> ClientEnv -> JSM (Either ClientError a)
runClientM cm env = runExceptT $ flip runReaderT env $ fromClientM cm

performRequest :: [Status] -> JSContextRef -> Request -> ClientM Response
performRequest _ jsmc req = do
  burl <- asks baseUrl
  waiter <- liftIO $ newEmptyMVar
  maybeBody <- case toBody req of
    Nothing -> pure Nothing
    Just body -> do
      -- Reason for copy: hopefully offset will be 0 and length b == len
      -- FIXME: use a typed array constructor that accepts offset and length and skip the copy
      b <- flip runJSM jsmc $ do
        (b, _offset, _len) <- JSaddle.ghcjsPure $ Buffer.fromByteString $ BS.copy $ L.toStrict body
        b' <- Buffer.thaw b
        JSaddle.ghcjsPure (Buffer.getArrayBuffer b')
      pure $ Just $ JS.pToJSVal b

  Miso.fetch
    (toUrl burl req)
    (Miso.ms $ T.decodeUtf8Lenient $ requestMethod req)
    maybeBody
    ((map (\mediaType -> ("Accept", Miso.ms $ renderHeader mediaType)) $
      toList $ requestAccept req) <>
     (maybe [] (\(_, mediaType) -> pure ("Content-Type", Miso.ms $ renderHeader mediaType)) $
      requestBody req) <>
     (map (\(k, v) -> (Miso.ms $ original k, Miso.ms v)) $ toList $ requestHeaders req)
    )
    (liftIO . putMVar waiter . Right)
    (liftIO . putMVar waiter . Left)
    Miso.ARRAY_BUFFER
    `runJSM` jsmc

  liftIO (takeMVar waiter) >>= either
    (\resp -> do
        body <- L.fromStrict . T.encodeUtf8 <$> (JS.fromJSValUnchecked (Miso.body resp) `runJSM` jsmc)
        throwError . mkFailureResponse burl req . fromMisoResponse $ resp { Miso.body = body }
    )
    (\resp -> do
        body <- flip runJSM jsmc $
          ArrayBuffer.unsafeFreeze (JS.pFromJSVal $ Miso.body resp) >>=
          JSaddle.ghcjsPure . Buffer.createFromArrayBuffer >>=
          fmap L.fromStrict . JSaddle.ghcjsPure . (Buffer.toByteString 0 Nothing)
        pure $ fromMisoResponse $ resp { Miso.body = body }
    )

toUrl :: BaseUrl -> Request -> JS.JSString
toUrl burl request =
  let pathS = JS.toJSString $ T.decodeUtf8Lenient $ L.toStrict $ toLazyByteString $
              requestPath request
      queryS =
          JS.toJSString $ T.decodeUtf8Lenient $
          renderQuery True $
          toList $
          requestQueryString request
  in JS.toJSString (showBaseUrl burl) <> pathS <> queryS :: JS.JSString

fromMisoResponse :: Miso.Response a -> ResponseF a
fromMisoResponse resp =
  Response
  { responseStatusCode = mkStatus (fromMaybe 0 $ Miso.status resp) ""
  , responseHeaders = Seq.fromList $
    map (\(a, b) -> (mk $ Miso.fromMisoString a, Miso.fromMisoString b)) $
    Map.toList $ Miso.headers resp
  , responseHttpVersion = http11 -- dummy value
  , responseBody = Miso.body resp
  }

mkFailureResponse :: BaseUrl -> Request -> ResponseF BSL.ByteString -> ClientError
mkFailureResponse burl request =
    FailureResponse (bimap (const ()) f request)
  where
    f b = (burl, BSL.toStrict $ toLazyByteString b)

toBody :: Request -> Maybe L.ByteString
toBody request = case requestBody request of
  Nothing -> Nothing
  Just (RequestBodyLBS "", _) -> Nothing
  Just (RequestBodyLBS x, _) -> Just x
  Just (RequestBodyBS "", _) -> Nothing
  Just (RequestBodyBS x, _)  -> Just $ L.fromStrict x
  Just (RequestBodySource _, _) -> error "RequestBodySource isn't supported"
