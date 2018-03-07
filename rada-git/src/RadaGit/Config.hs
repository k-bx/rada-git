{-# LANGUAGE DeriveGeneric #-}

module RadaGit.Config where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import GHC.Generics (Generic)

jsonOpts :: J.Options
jsonOpts = J.defaultOptions {J.fieldLabelModifier = J.camelTo2 '_'}

data Config = Config
  { projectsRssUrl :: Text
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = J.genericParseJSON jsonOpts

readConfig :: FilePath -> IO Config
readConfig path = do
  t <- BL.readFile path
  let cfg = either error id (J.eitherDecode t)
  return cfg
