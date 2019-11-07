{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Class
import Frankie.Server
import Frankie.Responses
import Frankie.Controller
import Frankie.IO ()

main :: IO ()
main = run 1

run :: Int -> IO ()
run nr = server 3000 "127.0.0.1" $ case nr of
          5 -> app5
          4 -> app4
          3 -> app3
          2 -> app2
          _ -> app1

ioPutStrLn :: String -> Controller config IO ()
ioPutStrLn str = lift $ putStrLn str

ioApp :: Controller () IO () -> Application IO
ioApp app = toApp app ()

app5 :: Application IO
app5 = ioApp $ do
  foos <- queryParams "foo"
  case foos :: [Int] of
   [x] -> do ioPutStrLn $ show x
             respond $ ok "text/plain" "yo"
   _   -> respond notFound

app4 :: Application IO
app4 = ioApp $ do
  request >>= ioPutStrLn . show
  host <- requestHeader "hOsT"
  ioPutStrLn $ show host
  woo <- requestHeader "woo"
  ioPutStrLn $ show $ woo == Nothing
  respond $ okJson "{}"
  ioPutStrLn $ "you wont see this"


app3 :: Application IO
app3 = ioApp $ do
  req <- request 
  ioPutStrLn $ show req
  respond $ okHtml "Yo controller!"

app2 :: Application IO
app2 _ = return (okHtml "Yo okHtml!")

app1 :: Application IO
app1 _ = return $ Response status200 [] "Hello World!"
