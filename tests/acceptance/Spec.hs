module Main (main) where

import Test.Hspec
import System.Environment

spec :: Spec
spec = do
    describe "foo" $ do
        it "works" $ do
            True `shouldBe` True

my_hspec :: IO ()
my_hspec = withArgs [] (hspec spec)

main :: IO ()
main = my_hspec
