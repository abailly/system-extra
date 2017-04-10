module System.BuildSpec where

import           Data.Monoid
import           System.Build
import           Test.Hspec

spec :: Spec
spec = describe "Build w/ Docker" $ do

  it "pass args to stack" $ do
    asStackArg (SimpleTarget "foo") `shouldBe` [":foo"]
    asStackArg (FullTarget "bar" "foo") `shouldBe` ["bar:exe:foo"]
    asStackArg (GHCOption "bar") `shouldBe` ["--ghc-options", "bar"]
    asStackArg (MoreArgs (SimpleTarget "bar") (GHCOption "foo"))
      `shouldBe` [":bar", "--ghc-options", "foo"]
    asStackArg (FullTarget "qix" "bar"
                <> GHCOption "foo"
                <> GHCOption "bar"
                <> mempty)
      `shouldBe` ["qix:exe:bar","--ghc-options","foo","--ghc-options","bar"]
