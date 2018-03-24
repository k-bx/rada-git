module RadaGit.Config where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import GHC.Generics (Generic)
import Network.URI
import Test.QuickCheck (Arbitrary(..))
import Network.URI.JSON ()

jsonOpts :: J.Options
jsonOpts = J.defaultOptions {J.fieldLabelModifier = J.camelTo2 '_'}

data Config = Config
  { projectsRssUrl :: URI
  , fsCache :: FilePath
  } deriving (Eq, Show, Generic)

instance Arbitrary Config where
  arbitrary =
    Config <$> pure (fromJust (parseURI "http://someurl.com/rss.xml")) <*>
    pure "/tmp/rada-git-cache"

instance FromJSON Config where
  parseJSON = J.genericParseJSON jsonOpts

instance ToJSON Config where
  toJSON = J.genericToJSON jsonOpts

readConfig :: FilePath -> IO Config
readConfig fpath = do
  t <- BL.readFile fpath
  let cfg = either error id (J.eitherDecode t)
  return cfg
