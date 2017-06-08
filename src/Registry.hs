{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Registry where
import           Config
import           Control.Lens hiding ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import Control.Monad.Reader
import           Data.Aeson.Lens        (key, values, _Integer, _String)
import           Data.Bool
import qualified Data.ByteString.Lazy   as BSL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Network.Wreq
import           System.Directory (getXdgDirectory, XdgDirectory(..), doesDirectoryExist, createDirectoryIfMissing)
import           System.FilePath
import           System.Posix.User
import System.Process

registryBaseUrl :: T.Text
registryBaseUrl = "https://registry-1.docker.io/v2/"



getLayers :: App [T.Text]
getLayers = do
  cfg <- ask
  let url = registryBaseUrl <> name cfg <> "/" <> "manifests" <> "/" <> tag cfg
  let opts = defaults  & header "Authorization" .~ ["Bearer " <> encodeUtf8 (authToken cfg)] & header "Accept" .~ ["application/vnd.docker.distribution.manifest.v2+json"]
  r <- liftIO (getWith opts (T.unpack url))
  return (r ^.. responseBody . key "layers" . values . key "digest" . _String )



downloadLayer :: FilePath -> T.Text -> App ()
downloadLayer path digest = do
  cfg <- ask
  let url = registryBaseUrl <> name cfg <> "/" <> "blobs" <> "/" <> digest
  let opts = defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 (authToken cfg)]
  r <- liftIO (getWith opts (T.unpack url))
  liftIO (BSL.writeFile path (r ^. responseBody))
  return ()

extractLayer :: FilePath -> App ()
extractLayer layerPath = do
  imageRoot <- asks imageRoot
  liftIO (createProcess (proc "tar" ["-x", "-f", layerPath, "-C", imageRoot]))
  -- liftIO (Tar.extract imageRoot layerPath)
  return ()

pull :: App FilePath
pull = do
  Config{..} <- ask
  alreadyDownloaded <- liftIO (doesDirectoryExist imagePath)
  if alreadyDownloaded
    then do
      logInfoN "image already pulled"
      return imagePath
    else do
      liftIO (createDirectoryIfMissing True imagePath)
      liftIO (createDirectoryIfMissing True imageRoot)
      layers <- getLayers
      forM_ layers (\digest -> do
        let layerPath = imagePath </> T.unpack (T.drop 7 digest) <.> "tar"
        logInfoN ("downloading layer: " <> digest)
        downloadLayer layerPath digest
        logInfoN ("extracting layer: " <> digest)
        extractLayer layerPath
        return ())

      return imagePath
