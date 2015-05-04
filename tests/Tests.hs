{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yusic
import Test.Hspec
-- import qualified Data.Text as T

main :: IO ()
main = hspec $ do
  describe "Verify that the note functions can be composed to create an automorphism" $ do
    it "Verify automorphism" $ do
      allNoteValid <- return noteAutomorphismP
      allNoteValid `shouldBe` True

  describe "Key guide automorphism" $ do
    it "Verify Key guide automorphism" $ do
      keyGuidesValid <- return keyGuideAutomorphismP
      keyGuidesValid `shouldBe` True

  describe "Note Midi automorphism" $ do
    it "Verify Note Midi automorphism" $ do
      allNoteMidiValid <- return noteMidiAutomorphismP
      allNoteMidiValid `shouldBe` True
