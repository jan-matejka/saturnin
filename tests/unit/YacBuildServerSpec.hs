module YacBuildServerSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
    describe "foo" $ do
        it "works" $ do
            True `shouldBe` True
