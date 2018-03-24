{-# LANGUAGE RecordWildCards #-}

module RadaGit.Projects where

import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.XML.Types
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (statusCode)
import Network.URI
import qualified Network.Wreq as W
import RadaGit.Cache
import RadaGit.Config
import RadaGit.Import
import Text.Groom
import Text.RSS.Import
import qualified Text.XML as XML

-- TODO: add retry logic
getWithRetryCaching :: FsCache -> W.Options -> URI -> IO BL.ByteString
getWithRetryCaching fscache opts uri = do
  fromCache <- getURLFromCache fscache uri
  case fromCache of
    Just rsp -> return rsp
    Nothing -> do
      rsp <- W.getWith opts (show uri)
      let sc = rsp ^. W.responseStatus & statusCode
      if sc == 200
        then return (rsp ^. W.responseBody)
        else error
               ("Got non-200 status code " ++ show sc ++ " for url " ++ show uri)

downloadProjects :: Config -> IO ()
downloadProjects Config {..} = do
  mgr <- newManager defaultManagerSettings
  let opts = W.defaults & W.manager .~ Right mgr
  fscache <- initCache fsCache
  -- TODO: have wiser invalidation
  rssLBS <- getWithRetryCaching fscache opts projectsRssUrl
  let doc = XML.toXMLDocument (XML.parseLBS_ def rssLBS)
  let (Just rss) = elementToRSS (documentRoot doc)
  putStrLn $ groom $ rss
  return ()
