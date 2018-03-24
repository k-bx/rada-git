module RadaGit.Tests where

import Data.Maybe (fromJust)
import qualified Data.Aeson as J
import Data.Aeson (FromJSON, ToJSON)
import RadaGit.Config
import RadaGit.Projects
import System.Environment (getArgs, withArgs)
import Test.Hspec
import Test.QuickCheck
import RadaGit.Import
import qualified Data.ByteString.Lazy as BL

runTests :: IO ()
runTests = do
  putStrLn "Running automatic tests"
  args' <- getArgs
  let args = tailDef [] args'
  withArgs args $ do
    hspec $ do
      describe "RadaGit.Config" $ do
        it "JSON roundtrips" $ do property (roundtripJsonTest @Config)
    hspec $ do
      describe "RadaGit.Projects" $ do
        it "parseURIWithCurr extracts a relative link with a base" $ do
          parseURIWithCurr (fromJust (parseURI "http://w1.c1.rada.gov.ua/pls/zweb2/webproc4_1?pf3511=63707"))
                           "webproc34?id=&pf3511=63707&pf35401=450492"
          `shouldBe` (parseURI "http://w1.c1.rada.gov.ua/pls/zweb2/webproc34?id=&pf3511=63707&pf35401=450492")
        it "extractDraftResolutionLink extracts a link" $ do
          bs <- BL.readFile "testdata/postanova_pf3511_63707.html"
          extractDraftResolutionLink (fromJust (parseURI "http://w1.c1.rada.gov.ua/pls/zweb2/webproc4_1?pf3511=63707")) bs
            `shouldReturn` (parseURI "http://w1.c1.rada.gov.ua/pls/zweb2/webproc34?id=&pf3511=63707&pf35401=450492")

roundtripJsonTest ::
     (Show a, Eq a, ToJSON a, FromJSON a, Arbitrary a) => a -> Property
roundtripJsonTest x = J.eitherDecode (J.encode x) === Right x
