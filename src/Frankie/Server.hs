{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
This module exposes a general interface for writing web apps. It's
slightly simpler than Wai, but also more general in allowing code to run in
an arbitrary monad (vs. IO). For example, this module exports an HTTP server
implementation in lio's 'DC' monad. Here is a simple example using this directly:

@
{-# LANGUAGE OverloadedStrings #-}
import LIO.HTTP.Server

main :: IO ()
main = server 3000 "127.0.0.1" app

app :: DCApplication
app _ = do
  return $ Response status200 [] "Hello World!"
@

The current 'Request' interface is deliberately simple in making all but the
request body pure. This may change since we may wish to label certain
headers (e.g., because they are sensitive) in the future and labeling a
whole request object may be too coarse grained.

Similarly, the 'Response' data type is deliberately simple. This may be
extended to make it easy to securely stream data in the future.
-}
module Frankie.Server (
  -- * Handle requests in the WebMonad
  WebMonad(..), Response(..),
  Application, Middleware,
  -- * General networking types 
  Port, HostPreference,
  module Network.HTTP.Types
  ) where
import Network.HTTP.Types
import Data.Text (Text)
import Control.Exception (SomeException)
import Network.Wai.Handler.Warp (Port, HostPreference)
import qualified Data.ByteString.Lazy as Lazy


-- | This type class is used to describe a general, simple Wai-like interface
-- for manipulating HTTP requests and running a web app server.
class Monad m => WebMonad m where
  -- | Data type representing the request.
  data Request m :: * 
  -- | HTTP request method.
  reqMethod      :: Request m -> Method            
  -- | HTTP version.
  reqHttpVersion :: Request m -> HttpVersion
  -- | Path info, i.e., the URL pieces after the scheme, host, port, not
  -- including the query string. Trailing slashes are ignored.
  reqPathInfo    :: Request m -> [Text]
  -- | Parsed query string.
  reqQueryString :: Request m -> Query
  -- | HTTP request headers.
  reqHeaders     :: Request m -> [Header]
  -- | HTTP request body.
  reqBody        :: Request m -> m Lazy.ByteString
  -- | Execute action and catch any exception.
  tryWeb         :: m a -> m (Either SomeException a)
  -- | Function for running the application on specified port and host info.
  server         :: Port -> HostPreference -> Application m -> IO ()

instance WebMonad m => Show (Request m) where
  show req = "Request {" ++
    "reqMethod = " ++ method ++
    ",reqHttpVersion = " ++ httpVersion ++
    ",reqPathInfo = " ++ pathInfo ++
    ",reqQueryString = " ++ queryString ++
    ",reqHeaders = " ++ headers ++
    ",reqBody = <NOT READ>}"
    where
        method      = show $ reqMethod req
        httpVersion = show $ reqHttpVersion req
        pathInfo    = show $ reqPathInfo req
        queryString = show $ reqQueryString req
        headers     = show $ reqHeaders req
        
-- | This data type encapsulates HTTP responses. For now, we only support lazy
-- ByteString bodies. In the future this data type may be extended to
-- efficiently support streams and file serving.
data Response = Response {
   rspStatus  :: Status          -- ^ HTTP response status.
 , rspHeaders :: [Header]        -- ^ HTTP response headers.
 , rspBody    :: Lazy.ByteString -- ^ HTTP response body
 } deriving (Eq, Show)

-- | An application is a function that takes an HTTP request and produces an
-- HTTP response, potentially performing side-effects.
type Application m = Request m -> m Response

-- | A middleware is simply code that transforms the request or response.
type Middleware m  = Application m -> Application m
