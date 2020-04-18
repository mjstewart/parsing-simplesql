module LexerSpec
  ( spec,
  )
where

import Test.Hspec

spec :: Spec
spec = do
  describe "add" $ do
    it "should add" $ do
      add 1 2 `shouldBe` 3


add :: Int -> Int -> Int
add x y = x + y
