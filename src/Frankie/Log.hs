{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |

module Frankie.Log where

import Data.Time.LocalTime (getZonedTime)
import System.IO
import System.Console.ANSI
import Control.Monad (when)
import Control.Monad.Trans
import Frankie.Config

-- | A logger is simply a function that takes the 'LogLevel' and string to
-- write, and produces an action which when executed may log the string. What
-- it means to log is by choice left up to the application.
newtype Logger m = Logger { runLogger :: LogLevel -> String -> m () }

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

class MonadLog m where
  log :: LogLevel -> String -> m ()

class HasLogger m a where
  getLogger :: a -> Logger m

instance (MonadTrans t, Monad m, HasLogger m a) => HasLogger (t m) a where
  getLogger x = let Logger logFun = getLogger x in
      Logger $ \ll str -> lift $ logFun ll str

instance (Monad m, HasLogger m config) => MonadLog (ConfigT config m) where
  log ll str = getLogger <$> getConfig >>= \logger -> runLogger logger ll str

class TTYLogger m where
  -- | Logger that prints to standard out, using color for TTYs.
  stdOutLogger :: Logger m
  -- | Logger that prints to standard err, using color for TTYs.
  stdErrLogger :: Logger m

instance TTYLogger IO where
  stdOutLogger = Logger $ \level str -> do
    isTTY <- hIsTerminalDevice stdout
    hLog isTTY stdout level str

  stdErrLogger = Logger $ \level str -> do
    isTTY <- hIsTerminalDevice stderr
    hLog isTTY stderr level str

-- | Logger that prints string to handle
hLog :: Bool -> Handle -> LogLevel -> String -> IO ()
hLog useColor h level str = do
  time <- getZonedTime
  if useColor
    then putColorStrLn level h $ show time ++ " " ++ show level ++ ": " ++ str
    else hPutStrLn h $ show time ++ " " ++ show level ++ ": " ++ str

-- | Print to handle, coloring the line according to the level.
putColorStrLn :: LogLevel -> Handle -> String -> IO ()
putColorStrLn level h str = do
  canColor <- hSupportsANSI h
  if canColor
    then do hSetSGR h [SetColor Background Dull Black,
                      SetColor Foreground Vivid (levelToColor level)]
            hPutStrLn h str
            hSetSGR h [Reset]
    else hPutStrLn h str

-- | Convert level to color
levelToColor :: LogLevel -> Color
levelToColor EMERGENCY = Red
levelToColor ALERT     = Red
levelToColor CRITICAL  = Red
levelToColor ERROR     = Red
levelToColor WARNING   = Yellow
levelToColor NOTICE    = Blue
levelToColor INFO      = White
levelToColor DEBUG     = Magenta

class FileLogger m where
  -- | Create a new logger that writes to the given path. Note that there
  -- is no clean way to clean up the file descriptor once the file is open.
  -- In general is is okay because we expect the logger to remain live for
  -- the lifetime of the application.
  openFileLogger :: FilePath -> IO (Logger m)

instance FileLogger IO where
  openFileLogger path = do
    handle <- openFile path AppendMode
    hSetBuffering handle LineBuffering
    return $ Logger $ hLog False handle

logAtLevel :: Monad m => LogLevel -> Logger m -> Logger m
logAtLevel level (Logger logger) = Logger $ \ll str -> when (ll < level) $ logger ll str

andLog :: Monad m => Logger m -> Logger m -> Logger m
andLog (Logger l1) (Logger l2) = Logger $ \ll str -> l1 ll str >> l2 ll str
