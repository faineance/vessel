{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Vessel where
import           Config
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Bool
import           Data.Monoid
import           Data.Text            as T
import           Registry
runApp :: Config -> App a -> IO a
runApp config =
      runStdoutLoggingT
    . flip runReaderT config
    . unApp



vessel :: App ()
vessel = do
  cfg <- ask

  let (_name, _tag) = T.breakOn ":" (imageName cfg)
  let name = if T.any (=='/') _name then _name else "library/" <> _name -- if it's an official image
  let tag = if T.null _tag then "latest" else T.drop 1 _tag -- remove : from tag name
  imagePath <- pull name tag
  -- $(logInfoSH) imagePath
  return ()
