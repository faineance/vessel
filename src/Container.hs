{-# LANGUAGE OverloadedStrings #-}
module Container where
import System.Process
import Control.Monad.Logger
import Data.Monoid
import Control.Monad.Reader
import Control.Monad.IO.Class
import Config
import           System.Posix.User

start :: App ()
start = do
  cfg <- ask
  if useOverlayFS cfg
    then undefined
    else do
      liftIO (createProcess (proc "cp" ["-av", imageRoot cfg, containerRoot cfg]))

      return undefined
