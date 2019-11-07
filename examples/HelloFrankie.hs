{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
import Prelude hiding (log)
import Frankie
import Frankie.Loggers
import Frankie.IO ()

main :: IO ()
main = do
  fileLogger <- openFileLogger "/tmp/frankie.log.0"
  runFrankieServer "prod" $ do
  mode "prod" $ do
    host "*"
    port 3030
    appConfig Config { configLogger = logAtLevel DEBUG stdErrLogger `andLog`
                                      logAtLevel INFO fileLogger }
      
  mode "dev" $ do
    host "127.0.0.1"
    port 3000
    appConfig Config { configLogger = stdErrLogger }
  --
  dispatch $ do
    get "/" top
    get "/a/b" top
    get "/fail" doFail
    get "/users/:uid" showUser
    get "/users/:uid/posts/:pid" showUserPost
    fallback $ do
      req <- request
      log WARNING $ "Not sure how to handle: " ++ show req
      respond $ notFound
  --
  onError $ \err -> do
    log ERROR $ "Controller failed with " ++ displayException err
    respond $ serverError "bad bad nab"

data Config = Config { configLogger :: Logger IO }

instance MonadController Config IO m => HasLogger m Config where
  getLogger config = liftLogger liftWeb $ configLogger config

top :: Controller s IO ()
top = respond $ okHtml "Woot"

showUser :: Int -> Controller Config IO ()
showUser uid = do
  log INFO $ "uid = " ++ show uid
  respond $ okHtml "showUser done!"

newtype PostId = PostId Int
   deriving Eq

instance Show PostId where
   show (PostId i) = show i

instance Parseable PostId where
  parseText t = case parseText t of
                  Just i | i > 0 -> Just (PostId i)
                  _ -> Nothing

showUserPost :: Int -> PostId -> Controller Config IO ()
showUserPost uid pid = do
  log INFO $ "uid = " ++ show uid
  log INFO $ "pid = " ++ show pid
  respond $ okHtml "showUserPost done!"

doFail :: Controller Config IO ()
doFail = do
  log DEBUG "about to throw an exception"
  fail "w00t"
