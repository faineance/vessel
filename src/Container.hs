{-# LANGUAGE OverloadedStrings #-}
module Container where
import           Config
-- import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Monoid
import           System.Linux.Mount
import           System.Posix.User
import           System.Process
import System.Posix.Process
import           LibC.Unshare
import Data.Text as T
copyFiles :: App ()
copyFiles =  do
    cfg <- ask

    liftIO (createProcess (proc "cp" ["-avf", imageRoot cfg, containerRoot cfg]))
    return ()

setMountPropagation :: App ()
setMountPropagation = do
  liftIO (mount "none" "/" "" [ReadOnly] noData)
  liftIO (makePrivate "/")

enterContainer :: App ()
enterContainer = do
  cfg <- ask
  let cmd  = containerArgs cfg
  liftIO (forkProcess $ executeFile (T.unpack cmd) False [] Nothing)

  return ()

start :: App ()
start = do
  copyFiles
  liftIO (unshare [PID, Network, Mount, UTC, CGroup, IPC, User])
  setMountPropagation
  enterContainer
  return ()
