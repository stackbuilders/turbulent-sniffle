{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
  where

-- turbulent-sniffle
import qualified Paths_turbulent_sniffle as Paths

-- base
import Control.Applicative ((<|>))
import qualified Data.Either as Either
import Data.Int (Int64)
import qualified Data.Maybe as Maybe

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString

-- directory
import qualified System.Directory as Directory

-- snap-core
import Snap.Core
  ( Method(GET, POST)
  , Snap
  )
import qualified Snap.Core as SnapCore
import qualified Snap.Util.FileServe as SnapFileServe
import Snap.Util.FileUploads
  ( PartInfo(..)
  , PartUploadPolicy
  , PolicyViolationException
  , UploadPolicy
  )
import qualified Snap.Util.FileUploads as SnapFileUploads

-- snap-server
import qualified Snap.Http.Server as SnapServer

-- transformers
import qualified Control.Monad.IO.Class as MonadIO


-- |
--
--

main :: IO ()
main =
  SnapServer.quickHttpServe (SnapCore.ifTop handleTop)


handleTop :: Snap ()
handleTop = do
  SnapCore.method GET handleTop'
    <|> SnapCore.method POST handleFilesUpload


-- |
--
--

handleTop' :: Snap ()
handleTop' = do
  indexHtml <- MonadIO.liftIO (Paths.getDataFileName "index.html")
  SnapFileServe.serveFile indexHtml


-- |
--
--

handleFilesUpload :: Snap ()
handleFilesUpload = do
  eitherFilesUpload <- handleFilesUpload'
  case eitherFilesUpload of
    Left reason -> do
      SnapCore.modifyResponse
        (SnapCore.setResponseStatus 400 "Bad Request")
      SnapCore.writeBS (ByteString.pack reason)
    Right response ->
      SnapCore.writeBS response


-- |
--
--

handleFilesUpload' :: Snap (Either String ByteString)
handleFilesUpload' = do
  temporaryDirectory <- MonadIO.liftIO Directory.getTemporaryDirectory
  SnapFileUploads.handleFileUploads
    temporaryDirectory
    uploadPolicy
    partUploadPolicy
    handleFilesRead


-- |
--
--

uploadPolicy :: UploadPolicy
uploadPolicy =
  SnapFileUploads.defaultUploadPolicy


-- |
--
--

partUploadPolicy
  :: PartInfo
  -> PartUploadPolicy
partUploadPolicy PartInfo{..} =
  if partContentType == "text/x-haskell" && Maybe.isJust partFileName
     then
       SnapFileUploads.allowWithMaximumSize maximumSize
     else
       SnapFileUploads.disallow


-- |
--
--

maximumSize :: Int64
maximumSize =
  1 * megabyte
  where
    megabyte =
      2 ^ (20 :: Int)


-- |
--
--

handleFilesRead
  :: [(PartInfo, Either PolicyViolationException FilePath)]
  -> Snap (Either String ByteString)
handleFilesRead pfs = do
  sd <- mapM handleFileRead (fmap snd pfs)
  case Either.partitionEithers sd of
    ([], contents) ->
      return (Right (ByteString.concat contents))
    (errors, _) -> do
      SnapCore.logError
        (ByteString.pack (unlines errors))
      return (Left (head errors))


-- |
--
--

handleFileRead
  :: Either PolicyViolationException FilePath
  -> Snap (Either String ByteString)
handleFileRead =
  either
    (return . Left . show)
    (MonadIO.liftIO . fmap Right . ByteString.readFile)
