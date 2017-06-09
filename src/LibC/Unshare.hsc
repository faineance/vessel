module LibC.Unshare (unshare, Namespace(..)) where
import Foreign
import Foreign.C
import Data.List (foldl')

#define _GNU_SOURCE
#include <sched.h>

foreign import ccall unsafe "unshare"
    c_unshare :: CInt -> IO CInt

data Namespace = PID | Network | Mount | UTC | CGroup | IPC | User
  deriving (Show, Eq)

namespacesToFlag :: [Namespace] -> CInt
namespacesToFlag namespaces = foldl' (.|.) 0 (map toFlag namespaces)
  where
    toFlag ns = case ns of
      PID -> (#const CLONE_NEWPID)
      Network -> (#const CLONE_NEWNET)
      Mount -> (#const CLONE_NEWNS)
      UTC -> (#const CLONE_NEWUTS)
      CGroup -> (#const CLONE_NEWCGROUP)
      IPC -> (#const CLONE_NEWIPC)
      User -> (#const CLONE_NEWUSER)

unshare :: [Namespace] -> IO ()
unshare namespaces = throwErrnoIfMinus1_ "unshare" $ c_unshare (namespacesToFlag namespaces)
