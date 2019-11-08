{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Frankie.Auth where

import Control.Monad.Trans
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Types
import Frankie.Controller
import Frankie.Config
import Frankie.Responses

data AuthMethod user m = AuthMethod {
  authMethodTry :: m (Maybe user),
  authMethodRequire :: m user
  }

class HasAuthMethod user m a | a -> user where
  getAuthMethod :: a -> AuthMethod user m

instance (MonadTrans t, Monad m, HasAuthMethod user m a) => HasAuthMethod user (t m) a where
  getAuthMethod x = let AuthMethod try req = getAuthMethod x in
      AuthMethod (lift try) (lift req)

class Monad m => MonadAuth user m | m -> user where
  tryAuth :: m (Maybe user)
  requireAuth :: m user

instance (Monad m, HasAuthMethod user m config) => MonadAuth user (ConfigT config m) where
  tryAuth = getAuthMethod <$> getConfig >>= authMethodTry
  requireAuth = getAuthMethod <$> getConfig >>= authMethodRequire

type Username = Text
type Password = Text

httpBasicAuth :: forall w m user. MonadController w m => (Username -> Password -> m (Maybe user)) -> AuthMethod user m
httpBasicAuth getUser = AuthMethod {
  authMethodTry = checkIfAuth,
  authMethodRequire = checkIfAuth >>= \case
      Just user -> pure user
      Nothing -> askAuth
  }
  where
    checkIfAuth :: m (Maybe user)
    checkIfAuth = do
      authHeader <- requestHeader hAuthorization
      case authHeader >>= parseBasicAuthHeader of
         Just (username, password) -> getUser username password
         Nothing -> pure Nothing

    askAuth :: m user
    askAuth = respond $ requireBasicAuth "this website"

parseBasicAuthHeader :: ByteString -> Maybe (Text, Text)
parseBasicAuthHeader =
  ByteString.stripPrefix "Basic " >=>
  rightToMaybe . Base64.decode >=>
  rightToMaybe . Text.decodeUtf8' >=>
  splitAtChar ':'
  where
    splitAtChar :: Char -> Text -> Maybe (Text, Text)
    splitAtChar c text =
      let (before, after) = Text.break (== c) text in do
      (_, after') <- Text.uncons after
      return (before, after')

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right x) = Just x
