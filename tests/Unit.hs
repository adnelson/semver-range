{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Main (main) where

import Data.Maybe
import Data.Either
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..), oneof)
import Data.Monoid

import Data.SemVer

-- | This instance seems to be missing :(
instance (Arbitrary a, Arbitrary b, Arbitrary c,
          Arbitrary d, Arbitrary e, Arbitrary f)
         => Arbitrary (a, b, c, d, e, f) where
  arbitrary = (,,,,,) <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

-- | Asserts that the first argument is a `Right` value equal to the second
-- argument.
shouldBeR :: (Show a, Show b, Eq b) => Either a b -> b -> IO ()
shouldBeR x y = do
  shouldSatisfy x isRight
  let Right x' = x
  x' `shouldBe` y

main :: IO ()
main = hspec $ do
  describe "semver parsing" $ do
    it "should parse a basic semver" $ do
      parseSemVer "1.2.3" `shouldBeR` semver 1 2 3
    it "should parse a semver with only major version" $ do
      parseSemVer "1" `shouldBeR` semver 1 0 0
    it "should parse a semver with only major and minor versions" $ do
      parseSemVer "1.2" `shouldBeR` semver 1 2 0
    it "should parse a semver with release tags" $ do
      parseSemVer "1.2.3-alpha" `shouldBeR` SemVer 1 2 3 ["alpha"]
    it "should parse a semver with multiple release tags" $ do
      parseSemVer "1.2.3-alpha.3" `shouldBeR` SemVer 1 2 3 ["alpha", "3"]

  describe "semver range parsing" $ do
    it "should parse a semver into an exact range" $ do
      parseSemVerRange "1.2.3" `shouldBeR` Eq (semver 1 2 3)
    it "should parse a semver with partial version into a range" $ do
      -- 1.2 =====> (>=1.2.0 <1.3)
      parseSemVerRange "1.2" `shouldBeR` (Geq (semver 1 2 0)
                                          `And` Lt (semver 1 3 0))
