-- | Filesystem-based (or other) cache interface. We want to be very
-- careful with the Goverment and query it as few times as possible

module RadaGit.Cache where

import Control.Monad.Catch
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Network.URI
import Path
import Path.IO

data FsCache = FsCache
  { cachePath :: Path Abs Dir
  }

-- | Creates a cache path if it doesn't exist
initCache :: FilePath -> IO FsCache
initCache fp = do
  d <- resolveDir' fp
  does <- doesDirExist d
  case does of
    True -> putStrLn $ "Using an existing cache directory: " ++ show d
    False -> do
      putStrLn $ "Creating a directory: " ++ show d
      ensureDir d
  return (FsCache d)

getURLFromCache :: FsCache -> URI -> IO (Maybe BL.ByteString)
getURLFromCache FsCache {..} uri = do
  u <- uriToCache uri
  let p = cachePath </> u
  e <- doesFileExist p
  case e of
    True -> do
      putStrLn $ "Returning a cached file: " ++ show p
      Just <$> BL.readFile (toFilePath p)
    False -> do
      putStrLn $ "File not found in cache: " ++ show p
      return Nothing

putURLToCache :: FsCache -> URI -> BL.ByteString -> IO ()
putURLToCache FsCache {..} uri bs = do
  u <- uriToCache uri
  let p = cachePath </> u
  BL.writeFile (toFilePath p) bs

-- TODO: test that uriToFilename . filenameToUri == id
uriToCache :: MonadThrow m => URI -> m (Path Rel File)
uriToCache =
  parseRelFile .
  T.unpack . T.replace "/" "__" . T.pack . show
