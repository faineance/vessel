
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Config where
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT,
                                       runReaderT)
import           Data.Text            as T


data Config = Config
  { imageName    :: T.Text
  , imageArgs    :: T.Text
  , useOverlayFs :: Bool
  } deriving (Show)

newtype App a = App
  { unApp :: ReaderT Config (LoggingT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadLogger)
