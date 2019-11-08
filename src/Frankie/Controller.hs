{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

{- | 'Controller' provides a convenient syntax for writting
   'Application' code as a Monadic action with access to an HTTP
   request as well as app specific data (e.g., a database connection
   pool, app configuration, etc.). The convenience comes from the
   helper functions that this module exports.  For example,
   'redirectBack' reads the underlying request to extract the referer
   and returns a redirect response:

  @
    myController = do
      ...
      if badLogin
        then 'redirectBack'
        else 'respond' $ 'okHtml' "w00t!"
  @
-}
module Frankie.Controller (
  MonadController(..),
  -- * Request relate accessors
  requestHeader,
  queryParams, Parseable(..),
  -- * Response related accessors
  redirectBack,
  redirectBackOr,
  -- * Internal controller monad
  fromApp, toApp,
  ControllerT(..),
  ControllerStatus(..),
  tryController,
  -- -- * DC Label specific type alias
  -- DCController
  ) where

import Prelude hiding (log)
import Control.Exception (SomeException)
import Frankie.Server
import Frankie.Responses

import Control.Applicative ()
import Control.Monad
import Control.Monad.Trans.Class

import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Text.Read (readMaybe)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


-- | This encodes the controller state. When 'Done', the controller will
-- short-circuit and produce the 'Response'. Otherwise the controller is still
-- running and has an intermediate result encoded by 'Working'.
data ControllerStatus a = Done Response
                        | Working a
                        deriving (Eq)

instance Functor ControllerStatus where
  fmap f cs = case cs of
    Working a -> Working $ f a
    Done r    -> Done r

-- | The Controller monad is used to encode stateful HTTP controller
-- computations.  The monad is a reader monad that provides the current request
-- (via 'request' or 'get'). It is also a state monad that theads the
-- application-specific state's' throughout the computation (accessible via
-- 'getAppState' and 'putAppState').
--
-- Within the Controller monad, the remainder of the computation can be
-- short-circuited by 'respond'ing with a 'Response'.
newtype ControllerT m a = ControllerT {
 runController :: Request m -> m (ControllerStatus a)
} deriving (Typeable)

instance Functor m => Functor (ControllerT m) where
  fmap f (ControllerT act) = ControllerT $ \req ->
    go `fmap` act req
    where go cs = f `fmap` cs

instance (Monad m, Functor m) => Applicative (ControllerT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ControllerT m) where
  return a = ControllerT $ \_ -> return $ Working a
  (ControllerT act0) >>= fn = ControllerT $ \req -> do
    cs <- act0 req
    case cs of
      Done resp -> return $ Done resp
      Working v -> do
        let (ControllerT act1) = fn v
        act1 req

instance MonadTrans ControllerT where
  lift act = ControllerT $ \_ -> act >>= \r -> return $ Working r

class (Monad m, WebMonad w) => MonadController w m | m -> w where
  request :: m (Request w)
  respond :: Response -> m a
  liftWeb :: w a -> m a

instance WebMonad m => MonadController m (ControllerT m) where
  request = ControllerT $ \req -> return $ Working req
  respond resp = ControllerT $ \_ -> return $ Done resp
  liftWeb = lift

-- | Try executing the controller action, returning the result or raised
-- exception. Note that exceptions restore the state.
tryController :: WebMonad m
              => ControllerT m a
              -> ControllerT m (Either SomeException a)
tryController ctrl = ControllerT $ \req -> do
  eres <- tryWeb $ runController ctrl req
  case eres of
   Left err -> return $ Working (Left err)
   Right stat ->
    case stat of
      Working a -> return $ Working (Right a)
      Done r    -> return $ Done r

-- | Convert an application to a controller. Internally, this uses
-- 'respond' to produce the response.
fromApp :: MonadController w m => Application w -> m ()
fromApp app = do
  req <- request
  resp <- liftWeb $ app req
  respond resp

-- | Convert the controller into an 'Application'. This can be used to
-- directly run the controller with 'server', for example.
toApp :: WebMonad m => ControllerT m () -> Application m
toApp ctrl req = do
  cs <- runController ctrl req
  return $ case cs of
            Done resp -> resp
            _         -> serverError "Unhandled case"

--
-- query parameters
--

-- | Looks up the parameters in the request's query string and returns the
-- @Parseable@ values or 'Nothing'.
--
-- For example, for a request with query string: \"?foo=bar&baz=7\",
-- @queryParam \"foo\"@
-- would return @["bar"]@, but
-- @queryParam \"zap\"@
-- would return @[]@.
queryParams :: (MonadController w m, Parseable a)
            => Strict.ByteString -- ^ Parameter name
            -> m [a]
queryParams varName = do
  query <- reqQueryString <$> request
  return $ mapMaybe go query
    where go (name, mparam) = if name == varName
                                then mparam >>= parseBS
                                else Nothing

-- | The class of types into which query parameters and path parts may
-- be converted. We provide definitions for both parse functions in
-- terms of the other, so only one definition is necessary.
class Typeable a => Parseable a where
  -- | Try parsing 'Strict.ByteString' as @a@.
  parseBS   :: Strict.ByteString -> Maybe a
  parseBS bs  = case Text.decodeUtf8' bs of
                  Left _  -> Nothing
                  Right t -> parseText t
  -- | Try parsing 'Text' as @a@.
  parseText :: Text -> Maybe a
  parseText = parseBS . Text.encodeUtf8

instance Parseable Strict.ByteString where
  parseBS   = Just
  parseText = Just . Text.encodeUtf8
instance {-# INCOHERENT #-} Parseable String where
  parseBS   = Just . Char8.unpack
  parseText = Just . Text.unpack
instance Parseable Text where
  parseBS bs  = case Text.decodeUtf8' bs of
                  Left _  -> Nothing
                  Right t -> Just t
  parseText = Just
instance {-# OVERLAPPABLE #-} (Read a, Typeable a) => Parseable a where
  parseBS   = readMaybe . Char8.unpack
  parseText = readMaybe . Text.unpack

-- | Returns the value of the given request header or 'Nothing' if it is not
-- present in the HTTP request.
requestHeader :: MonadController w m
              => HeaderName -> m (Maybe Strict.ByteString)
requestHeader name = lookup name . reqHeaders <$> request

-- | Redirect back to the referer. If the referer header is not present
-- 'redirectTo' root (i.e., @\/@).
redirectBack :: MonadController w m => m ()
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back to the referer. If the referer header is not present
-- fallback on the given 'Response'.
redirectBackOr :: MonadController w m
               => Response -- ^ Fallback response
               -> m ()
redirectBackOr def = do
  mrefr <- requestHeader "referer"
  case mrefr of
    Just refr -> respond $ redirectTo refr
    Nothing   -> respond def
