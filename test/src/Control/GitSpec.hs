{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.GitSpec where

import Test.Hspec
import Test.Hspec.Glitter

import Control.Git

spec :: Spec
spec = do
  describe "parseGitProcelain" do
    it "should parse examples" do
      parseGitProcelain ""
        `shouldBe` Right []
      parseGitProcelain " M test.hs\n"
        `shouldBe` Right [GitStatus Unmodified Modified Nothing "test.hs"]
      parseGitProcelain " M test.hs\n ? test2.hs\n"
        `shouldBe` Right
          [ GitStatus Unmodified Modified Nothing "test.hs"
          , GitStatus Unmodified Untracked Nothing "test2.hs"
          ]
      parseGitProcelain " M test.hs -> test2.hs\n"
        `shouldBe` Right [GitStatus Unmodified Modified (Just "test.hs") "test2.hs"]
      parseGitProcelain " M \"test.\\\\hs\" -> test2.hs\n"
        `shouldBe` Right [GitStatus Unmodified Modified (Just "test.\\hs") "test2.hs"]
