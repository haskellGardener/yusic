{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yusic
import Test.Hspec
-- import qualified Data.Text as T

main :: IO ()
main = hspec $ do
  describe "Verify that the note functions can be composed to create an endomorphism" $ do
    it "Verify endomorphism" $ do
      allNoteValid <- return noteEndomorphismP
      allNoteValid `shouldBe` True

  describe "Key guide endomorphism" $ do
    it "Verify Key guide endomorphism" $ do
      keyGuidesValid <- return keyGuideEndomorphismP
      keyGuidesValid `shouldBe` True

  describe "Note Midi endomorphism" $ do
    it "Verify Note Midi endomorphism" $ do
      allNoteMidiValid <- return noteMidiEndomorphismP
      allNoteMidiValid `shouldBe` True
