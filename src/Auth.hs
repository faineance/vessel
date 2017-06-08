{-# LANGUAGE OverloadedStrings #-}
module Auth
    (
    getAuthToken
    ) where

import Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT,
                                   runReaderT)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson.Lens        (key, values, _Integer, _String)
import           Data.Text            as T
import           Network.Wreq
import           System.Directory (getXdgDirectory, XdgDirectory(..), doesDirectoryExist, createDirectoryIfMissing)
import           System.FilePath
import           System.Posix.User
import Data.Monoid
import Data.Bool
authBaseUrl :: T.Text
authBaseUrl = "https://auth.docker.io/token"

getAuthToken :: T.Text -> IO T.Text
getAuthToken name  = do
  r <- liftIO (getWith opts (T.unpack authBaseUrl))
  return (r ^. responseBody . key "token" . _String)
  where
    opts = defaults
        & param "service" .~ ["registry.docker.io"]
        & param "scope" .~  [ "repository:" <> name <> ":pull"]
