{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

import Prelude hiding (id, (.))

import Control.Category
import Control.Monad.Except
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Lazy
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route
import Obelisk.Route.TH

badEncoder
  :: ( Binary a
     , Applicative check
     , MonadError Text parse
     )
  => Encoder check parse a ByteString
badEncoder = unsafeEncoder $ pure $ EncoderImpl
  { _encoderImpl_encode = Base64URL.encode . Data.ByteString.Lazy.toStrict . Binary.encode
  , _encoderImpl_decode = \b64 -> case Base64URL.decode b64 of
      Left e -> throwError $ "badEncoder: failed to decode base64: " <> T.pack e
      Right raw -> case Binary.decodeOrFail $ Data.ByteString.Lazy.fromStrict raw of
        Left e -> throwError $ "badEncoder: failed to decode Binary data: " <> T.pack (show e)
        Right ("", _, result) -> pure result
        Right (leftover, _, _) -> throwError $ "badEncoder: leftover Binary data after parsing: " <> T.pack (show leftover)
  }

goodEncoder
  :: ( Binary a
     , Applicative check
     , MonadError Text parse
     )
  => Encoder check parse a ByteString
goodEncoder = base64UrlEncoder . bytestringToStrictEncoder . binaryEncoder

base64UrlEncoder
  :: ( Applicative check
     , MonadError Text parse
     )
  => Encoder check parse ByteString ByteString
base64UrlEncoder = unsafeEncoder $ pure $ EncoderImpl
  { _encoderImpl_encode = Base64URL.encode
  , _encoderImpl_decode = \b64 -> case Base64URL.decode b64 of
      Left e -> throwError $ "base64UrlEncoder: failed to decode: " <> T.pack e
      Right raw -> pure raw
  }

bytestringToStrictEncoder
  :: ( Applicative check
     , Applicative parse
     )
  => Encoder check parse Data.ByteString.Lazy.ByteString ByteString
bytestringToStrictEncoder = unsafeEncoder $ pure $ EncoderImpl
  { _encoderImpl_encode = Data.ByteString.Lazy.toStrict
  , _encoderImpl_decode = pure . Data.ByteString.Lazy.fromStrict
  }

binaryEncoder
  :: ( Binary a
     , Applicative check
     , MonadError Text parse
     )
  => Encoder check parse a Data.ByteString.Lazy.ByteString
binaryEncoder = unsafeEncoder $ pure $ EncoderImpl
  { _encoderImpl_encode = Binary.encode
  , _encoderImpl_decode = \raw -> case Binary.decodeOrFail raw of
      Left e -> throwError $ "binaryEncoder: failed to decode Binary data: " <> T.pack (show e)
      Right ("", _, result) -> pure result
      Right (leftover, _, _) -> throwError $ "binaryEncoder: leftover Binary data after parsing: " <> T.pack (show leftover)
  }

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty)
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
