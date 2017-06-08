{-# LANGUAGE OverloadedStrings #-}

module Vessel where
import           Config
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Bool
import           Data.Monoid
import           Data.Text            as T
import           Registry
import Container
runApp :: Config -> App a -> IO a
runApp config =
      runStdoutLoggingT
    . flip runReaderT config
    . unApp

vessel :: App ()
vessel = do
  pull
  start
  return ()
