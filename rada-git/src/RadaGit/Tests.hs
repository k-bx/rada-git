{-# LANGUAGE TypeApplications #-}

module RadaGit.Tests where

import qualified Data.Aeson as J
import Data.Aeson (FromJSON, ToJSON)
import RadaGit.Config
import System.Environment (getArgs, withArgs)
import Test.Hspec
import Test.QuickCheck

runTests :: IO ()
runTests = do
  putStrLn "Running automatic tests"
  args <- getArgs
  withArgs (tail args) $ do
    hspec $ do
      describe "RadaGit.Config" $ do
        it "JSON roundtrips" $ do property (roundtripJsonTest @Config)

roundtripJsonTest ::
     (Show a, Eq a, ToJSON a, FromJSON a, Arbitrary a) => a -> Property
roundtripJsonTest x = J.eitherDecode (J.encode x) === Right x
