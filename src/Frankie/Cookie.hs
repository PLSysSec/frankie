{-# LANGUAGE FlexibleContexts #-}

-- | Basic cookie support.

module Frankie.Cookie ( -- * Set and get cookies
                        -- ** Set cookies
                        setCookie
                      , cookie
                      , defaultSetCookie
                        -- ** Get cookies
                      , getCookies
                      , getCookie
                        -- * Web Cookie interface
                      , module Web.Cookie
                      ) where


import qualified Web.Cookie as C
import Web.Cookie hiding ( parseSetCookie
                         , renderSetCookie
                         , defaultSetCookie
                         , def
                         , parseCookies
                         , renderCookies
                         , parseCookiesText
                         , renderCookiesText
                         )

import Frankie.Server
import Frankie.Controller
import Network.HTTP.Types.Header
import Data.Binary.Builder (toLazyByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy


-- | Simple cookie using default settings.
cookie :: Strict.ByteString -> Strict.ByteString -> SetCookie
cookie n v = defaultSetCookie { setCookieName = n, setCookieValue = v }

-- | Default set cookie with secure defaults.
defaultSetCookie :: SetCookie
defaultSetCookie = C.defaultSetCookie {
                     setCookieHttpOnly = True
                   , setCookieSecure   = True
                   , setCookieSameSite = Just sameSiteStrict }

-- | Add set-cookie response header to response.
setCookie :: SetCookie -> Response -> Response
setCookie sc resp = 
  let headers = rspHeaders resp
      scS8 = Lazy.toStrict $ toLazyByteString $ C.renderSetCookie sc
  in resp { rspHeaders = (hSetCookie, scS8) : headers }

-- | Get cookies
getCookies :: MonadController w m => m Cookies
getCookies = do
  mcs <- requestHeader hCookie
  case mcs of
    Nothing -> return []
    Just cs -> return $ C.parseCookies cs

-- | Get particular cookie name.
getCookie :: MonadController w m
          => Strict.ByteString       -- ^ Cookie name
          -> m ([Strict.ByteString]) -- ^ Cookie values
getCookie name = do
  cs <- getCookies
  return $ map snd $ filter (\(n,_) -> n == name) cs
