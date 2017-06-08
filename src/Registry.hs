{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Registry where
import           Config
import           Control.Lens hiding ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
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

authBaseUrl, registryBaseUrl :: T.Text
authBaseUrl = "https://auth.docker.io/token"
registryBaseUrl = "https://registry-1.docker.io/v2/"


getAuthToken :: T.Text -> App T.Text
getAuthToken name  = do
  r <- liftIO (getWith opts (T.unpack authBaseUrl))
  return (r ^. responseBody . key "token" . _String)
  where
    opts = defaults
        & param "service" .~ ["registry.docker.io"]
        & param "scope" .~  [ "repository:" <> name <> ":pull"]

getLayers :: T.Text -> T.Text -> T.Text -> App [T.Text]
getLayers token name tag = do
  r <- liftIO (getWith opts (T.unpack url))
  return (r ^.. responseBody . key "layers" . values . key "digest" . _String )
  where
    url = registryBaseUrl <> name <> "/" <> "manifests" <> "/" <> tag
    opts = defaults
        & header "Authorization" .~ ["Bearer " <> encodeUtf8 token]
        & header "Accept" .~ ["application/vnd.docker.distribution.manifest.v2+json"]


downloadLayer :: T.Text -> FilePath -> T.Text -> T.Text -> App ()
downloadLayer token path name digest = do
  r <- liftIO (getWith opts (T.unpack url))
  liftIO (BSL.writeFile path (r ^. responseBody))
  return ()
  where
    url = registryBaseUrl <> name <> "/" <> "blobs" <> "/" <> digest
    opts = defaults
        & header "Authorization" .~ ["Bearer " <> encodeUtf8 token]

extractLayer :: FilePath -> FilePath -> App ()
extractLayer layerPath imageRoot = do
  liftIO (createProcess (proc "tar" ["-x", "-f", layerPath, "-C", imageRoot]))
  -- liftIO (Tar.extract imageRoot layerPath)
  return ()

getAppRoot :: App FilePath
getAppRoot = liftIO (isRoot >>= bool (getXdgDirectory XdgData "vessel") (return "/var/lib/vessel"))
    where
      isRoot = fmap (== 0) getRealUserID

pull :: T.Text -> T.Text -> App FilePath
pull name tag = do
  token <- getAuthToken name
  appRoot <- getAppRoot
  let imagePath = appRoot </> T.unpack (name <> ":" <> tag)
  let imageRoot = imagePath </> "root"
  alreadyDownloaded <- liftIO (doesDirectoryExist imagePath)
  if alreadyDownloaded
    then do
      logInfoN "image already pulled"
      return imagePath
    else do
      liftIO (createDirectoryIfMissing True imagePath)
      liftIO (createDirectoryIfMissing True imageRoot)
      layers <- getLayers token name tag
      forM_ layers (\digest -> do
        let layerPath = imagePath </> T.unpack (T.drop 7 digest) <.> "tar"
        logInfoN ("downloading layer: " <> digest)
        downloadLayer token layerPath name digest
        logInfoN ("extracting layer: " <> digest)
        extractLayer layerPath (imagePath </> "root")
        return ())

      return imagePath
