module RadaGit.Projects where

import Codec.Text.IConv
import Control.Concurrent (threadDelay)
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.String.Class as S
import qualified Data.Text as T
import Data.XML.Types
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (statusCode)
import Network.URI
import qualified Network.Wreq as W
import RadaGit.Cache
import RadaGit.Config
import RadaGit.Import
import Text.HTML.Scalpel.Core
import Text.RSS.Import
import Text.RSS.Syntax
import qualified Text.XML as XML

-- TODO: add retry logic
getWithRetryCaching :: FsCache -> W.Options -> URI -> IO BL.ByteString
getWithRetryCaching fscache opts uri = do
  fromCache <- getURLFromCache fscache uri
  case fromCache of
    Just rsp -> do
      putStrLn "Got file from cache"
      return rsp
    Nothing -> do
      putStrLn "Couldn't get a file from cache, downloading"
      rsp <- W.getWith opts (show uri)
      -- safety measures
      threadDelay 1000000
      let sc = rsp ^. W.responseStatus & statusCode
      if sc == 200
        then do
          let body = rsp ^. W.responseBody
          putURLToCache fscache uri body
          return body
        else error
               ("Got non-200 status code " ++ show sc ++ " for url " ++ show uri)

fromWin :: BL.ByteString -> BL.ByteString
fromWin = convert "CP1251" "UTF-8"

-- | Extends parseURI to handle relative URIs from current page
parseURIWithCurr ::
     URI -- ^ current page URI (or base URI), last part will be trimmed
  -> String -- ^ URI to parse
  -> Maybe URI
parseURIWithCurr base u =
  parseURI u <|>
  parseURI
    (show
       (base
        { uriPath = removeLastPiece (uriPath base)
        , uriQuery = ""
        , uriFragment = ""
        }) ++
     "/" ++ u)
  where
    removeLastPiece =
      T.unpack . T.intercalate "/" . dropLast . T.splitOn "/" . T.pack
    dropLast xs = take (length xs - 1) xs

extractDraftResolutionLink ::
     HasCallStack => URI -> BL.ByteString -> IO (Maybe URI)
extractDraftResolutionLink currUri bs = do
  let mres = scrapeStringLike bs parser
  putStrLn $ "Scraped: " ++ show mres
  case mres of
    Nothing -> putStrLn "No project link found" >> pure Nothing
    Just res -> pure res
  where
    parser :: Scraper BL.ByteString (Maybe URI)
    parser = do
      links <-
        chroots
          ("div" @: [hasClass "zp-info"] // "a")
          ((,) <$> (attr "href" "a") <*> text "a")
      let mlink =
            listToMaybe
              (filter
                 (\(_href, cont) ->
                    "Проект Постанови" `T.isInfixOf` (S.toText cont))
                 links)
      -- TODO: safer
      return (mlink <&> fst <&> S.toString <&> parseURIWithCurr currUri & join)

-- | Download the Draft Resolution Doc from the Project Page like http:\/\/w1.c1.rada.gov.ua\/pls\/zweb2\/webproc4_1?pf3511=63707
processProjItemPage :: FsCache -> W.Options -> URI -> BL.ByteString -> IO ()
processProjItemPage fscache opts currUri bs = do
  muri <- extractDraftResolutionLink currUri bs
  case muri of
    Nothing -> do
      putStrLn "Couldn't extract a URI for the project's doc file"
    Just uri -> do
      _docBs <- getWithRetryCaching fscache opts uri
      return ()
  return ()

downloadProjects :: Config -> IO ()
downloadProjects Config {..} = do
  mgr <- newManager defaultManagerSettings
  let opts = W.defaults & W.manager .~ Right mgr
  fscache <- initCache fsCache
  let currUri = projectsRssUrl
  -- TODO: have wiser invalidation
  rssLBS <- getWithRetryCaching fscache opts projectsRssUrl
  let doc = XML.toXMLDocument (XML.parseLBS_ def (fromWin rssLBS))
  let (Just rss) = elementToRSS (documentRoot doc)
  let items = rssItems (rssChannel rss)
  forM_ items $ \item -> do
    let ml = rssItemLink item
    case ml of
      Nothing -> putStrLn "Project document's rss item has no link attached"
      Just l ->
        case parseURI (S.toString l) of
          Nothing ->
            putStrLn $ "Couldn't parse a URI from RSS: " ++ S.toString l
          Just uri -> do
            projItemPageBs <- getWithRetryCaching fscache opts uri <&> fromWin
            processProjItemPage fscache opts currUri projItemPageBs
  return ()
