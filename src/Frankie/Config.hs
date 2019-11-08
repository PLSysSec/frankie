{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module Frankie.Config where

import Control.Monad.Reader
import Frankie.Controller

newtype ConfigT config m a = ConfigT { unConfigT :: ReaderT config m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

configure :: config -> ConfigT config m a -> m a
configure cfg = flip runReaderT cfg . unConfigT

class Monad m => MonadConfig config m | m -> config where
  getConfig :: m config

instance Monad m => MonadConfig config (ConfigT config m) where
  getConfig = ConfigT ask

instance MonadController w m => MonadController w (ConfigT config m) where
  request = lift request
  respond resp = lift $ respond resp
  liftWeb act = lift $ liftWeb act
