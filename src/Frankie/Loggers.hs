{-# LANGUAGE Unsafe #-}

{- | This module exports a set of loggers in the IO monad. We mark this
   module as @Unsafe@ since it allows for writing to the console and
   files without checking labels.
-}

module Frankie.Loggers (
  -- * TTY loggers
  TTYLogger(..),
  -- * File loggers
  FileLogger(..)
) where

import Frankie
import Data.Time.LocalTime (getZonedTime)
import System.IO
import Control.Monad.Trans.Class (lift)
import System.Console.ANSI

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
  openFileLogger :: FrankieConfigMonad k => FilePath -> k s m (Logger m)

instance FileLogger IO where
  openFileLogger path = liftFrankie $ FrankieConfig $ lift $ do
    handle <- openFile path AppendMode
    hSetBuffering handle LineBuffering
    return $ Logger $ hLog False handle
