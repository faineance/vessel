
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings,NamedFieldPuns          #-}
module Config where
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
import Auth

data Args = Args
  { imageName    :: T.Text
  , _imageArgs    :: T.Text
  , _useOverlayFS :: Bool
  } deriving (Show)

data Config = Config
  { name    :: T.Text
  , tag    :: T.Text
  , containerArgs :: T.Text
  , containerRoot :: FilePath
  , appRoot :: FilePath
  , imagePath :: FilePath
  , imageRoot :: FilePath
  , authToken :: T.Text
  , useOverlayFS :: Bool
  } deriving (Show)

newtype App a = App
  { unApp :: ReaderT Config (LoggingT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadLogger)

getConfig :: Args -> IO Config
getConfig (Args imageName containerArgs useOverlayFS) = do
  appRoot <- getAppRoot
  let (_name, _tag) = T.breakOn ":" imageName
  let name = if T.any (=='/') _name then _name else "library/" <> _name -- if it's an official image
  let tag = if T.null _tag then "latest" else T.drop 1 _tag -- remove : from tag name
  let imagePath = appRoot </> "images" </> T.unpack (name <> ":" <> tag)
  let imageRoot = imagePath </> "root"
  let containerRoot = appRoot </> "containers" </> (unpack _name <> "-" <> "todo")
  authToken <- getAuthToken name
  return Config { name, tag, containerArgs, containerRoot, appRoot, imagePath, imageRoot, useOverlayFS, authToken}

getAppRoot :: IO FilePath
getAppRoot = isRoot >>= bool (getXdgDirectory XdgData "vessel") (return "/var/lib/vessel")
    where
      isRoot = fmap (== 0) getRealUserID
