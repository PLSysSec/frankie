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
  -- * App-specific logging
  Logger(..), LogLevel(..),
  MonadLog(..), HasLogger(..),
  -- * Internal controller monad
  fromApp, toApp,
  Controller(..),
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
data Controller config m a = Controller {
 runController :: config -> Request m -> m (ControllerStatus a)
} deriving (Typeable)

instance Functor m => Functor (Controller config m) where
  fmap f (Controller act) = Controller $ \cfg req ->
    go `fmap` act cfg req
    where go cs = f `fmap` cs

instance (Monad m, Functor m) => Applicative (Controller config m) where
  pure = return
  (<*>) = ap

class MonadLog m where
  log :: LogLevel -> String -> m ()

class HasLogger m a where
  getLogger :: a -> Logger m

instance (MonadController config w m, HasLogger m config) => MonadLog m where
  log ll str = do
    (Logger logf) <- getLogger <$> getConfig
    logf ll str

instance Monad m => Monad (Controller s m) where
  return a = Controller $ \_ _ -> return $ Working a
  (Controller act0) >>= fn = Controller $ \cfg req -> do
    cs <- act0 cfg req
    case cs of
      Done resp -> return $ Done resp
      Working v -> do
        let (Controller act1) = fn v
        act1 cfg req

instance MonadTrans (Controller s) where
  lift act = Controller $ \_ _ -> act >>= \r -> return $ Working r

class (Monad m, WebMonad w) => MonadController config w m | m -> config w where
  getConfig :: m config
  request :: m (Request w)
  respond :: Response -> m a
  liftWeb :: w a -> m a

instance WebMonad w => MonadController config w (Controller config w) where
  getConfig = Controller $ \cfg _ -> return $ Working cfg
  request = Controller $ \_ req -> return $ Working req
  respond resp = Controller $ \_ _ -> return $ Done resp
  liftWeb = lift

-- | Try executing the controller action, returning the result or raised
-- exception. Note that exceptions restore the state.
tryController :: WebMonad m
              => Controller s m a
              -> Controller s m (Either SomeException a)
tryController ctrl = Controller $ \cfg req -> do
  eres <- tryWeb $ runController ctrl cfg req
  case eres of
   Left err -> return $ Working (Left err)
   Right stat ->
    case stat of
      Working a -> return $ Working (Right a)
      Done r    -> return $ Done r

-- | Convert an application to a controller. Internally, this uses
-- 'respond' to produce the response.
fromApp :: MonadController s w m => Application w -> m ()
fromApp app = do
  req <- request
  resp <- liftWeb $ app req
  respond resp

-- | Convert the controller into an 'Application'. This can be used to
-- directly run the controller with 'server', for example.
toApp :: WebMonad m => Controller config m () -> config -> Application m
toApp ctrl cfg req = do
  cs <- runController ctrl cfg req
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
queryParams :: (MonadController config w m, Parseable a)
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
requestHeader :: WebMonad m
              => HeaderName -> Controller config m (Maybe Strict.ByteString)
requestHeader name = lookup name . reqHeaders <$> request

-- | Redirect back to the referer. If the referer header is not present
-- 'redirectTo' root (i.e., @\/@).
redirectBack :: WebMonad m => Controller s m ()
redirectBack = redirectBackOr (redirectTo "/")

-- | Redirect back to the referer. If the referer header is not present
-- fallback on the given 'Response'.
redirectBackOr :: WebMonad m
               => Response -- ^ Fallback response
               -> Controller s m ()
redirectBackOr def = do
  mrefr <- requestHeader "referer"
  case mrefr of
    Just refr -> respond $ redirectTo refr
    Nothing   -> respond def

-- -- | Log text using app-specific logger.
-- log :: WebMonad m => LogLevel -> String -> Controller s m ()
-- log level str = Controller $ \s0 (Logger logger) _ -> do
--    logger level str
--    return (Working (), s0)

-- | A logger is simply a function that takes the 'LogLevel' and string to
-- write, and produces an action which when executed may log the string. What
-- it means to log is by choice left up to the application.
newtype Logger m = Logger (LogLevel -> String -> m ())

-- | Severity of logging inforamation following RFC5424.
data LogLevel = EMERGENCY
              | ALERT
              | CRITICAL
              | ERROR
              | WARNING
              | NOTICE
              | INFO
              | DEBUG 
              deriving (Show, Eq, Ord)
