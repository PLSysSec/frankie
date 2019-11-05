{-| This module exposes an IO implementation of WebMonad.

@
{-# LANGUAGE OverloadedStrings #-}
import Frankie.Server
import Frankie.IO

main :: IO ()
main = server 3000 "127.0.0.1" app

app :: Application IO
app _ = do
  return $ Response status200 [] "Hello World!"
@
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Frankie.IO () where

import Control.Exception (try, toException)
import qualified Data.Text as Text
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Frankie.Server

instance WebMonad IO where
  data Request IO = RequestIO { unRequestIO :: Wai.Request }
  reqMethod      = Wai.requestMethod . unRequestIO
  reqHttpVersion = Wai.httpVersion . unRequestIO
  reqPathInfo    = Wai.pathInfo . unRequestIO
  reqQueryString = Wai.queryString . unRequestIO
  reqHeaders     = Wai.requestHeaders . unRequestIO
  reqBody        = Wai.strictRequestBody . unRequestIO
  tryWeb act     = do er <- try act
                      case er of
                        Left e -> return . Left . toException $ e
                        r -> return r
  server port hostPref app =
    let settings = Warp.setHost hostPref $ Warp.setPort port $
                   Warp.setServerName "frankie" $ Warp.defaultSettings
    in Warp.runSettings settings $ toWaiApplication app

-- | Internal function for converting a Frankie Application to a Wai Application
toWaiApplication :: Application IO -> Wai.Application
toWaiApplication app wReq wRespond = do
  resp <- app $ req
  wRespond $ toWaiResponse resp
    where req :: Request IO
          req = let pI0 = Wai.pathInfo wReq
                    pI1 = if (not . null $ pI0) && (last pI0 == Text.empty)
                            then init pI0
                            else pI0
                in RequestIO $ wReq { Wai.pathInfo = pI1 }
          toWaiResponse :: Response -> Wai.Response
          toWaiResponse (Response status headers body) = Wai.responseLBS status headers body
