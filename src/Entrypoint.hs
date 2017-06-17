{-# LANGUAGE OverloadedStrings #-}
module Entrypoint where
import           Data.Bool
import           Data.Semigroup           ((<>))
import           Data.Text                as T
import           Options.Applicative
import           Options.Applicative.Text
import           System.Directory
import           System.FilePath
import System.Exit
import Foreign.C

import           Config
import           Registry
import           Vessel



args :: Parser Args
args = Args
      <$> textArgument (metavar "IMAGE" <> help "Image name")
      <*> textArgument (metavar "ARGS" <> help "Image args")
      <*> switch
          ( long "overlayfs"
         <> short 'o'
         <> help "Use overlayfs to build root fs ( more efficient since it only stores deltas, but requries root. )" )

foreign export ccall entrypoint :: IO ()
entrypoint :: IO ()
entrypoint = do
  a <- execParser opts
  cfg <- getConfig a
  runApp cfg vessel
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Download and run docker images"
     <> header "vessel - container runtime" )
