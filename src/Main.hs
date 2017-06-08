{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.Bool
import           Data.Semigroup           ((<>))
import           Data.Text                as T
import           Options.Applicative
import           Options.Applicative.Text
import           System.Directory
import           System.FilePath
import           System.Posix.User

import           Config
import           Registry
import           Vessel

args :: Parser Config
args = Config
      <$> textArgument (metavar "IMAGE" <> help "Image name")
      <*> textArgument (metavar "ARGS" <> help "Image args")
      <*> switch
          ( long "overlayfs"
         <> short 'o'
         <> help "Use overlayfs to build root fs ( more efficient since it only stores deltas, but requries root. )" )


isRoot :: IO Bool
isRoot = fmap (== 0) getRealUserID


main :: IO ()
main = flip runApp vessel =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Download and run docker images"
     <> header "vessel - container runtime" )



--
-- main :: IO ()
-- main = do
--
--   -- let args =
--   directory <- isRoot >>= bool (getXdgDirectory XdgData "vessel") (return "/var/lib/vessel")
--   putStrLn directory
