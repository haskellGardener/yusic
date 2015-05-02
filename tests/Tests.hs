{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yusic
import Test.Hspec
-- import qualified Data.Text as T

main :: IO ()
main = hspec $ do
  describe "Verify that yusic outputs the correct data" $ do
    it "equals Dolly" $ do
      theSum <- return yusic
      theSum `shouldBe` "Well Hello Dolly!"
