{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}


module Main (main) where

import ClassyPrelude
import Data.Either (isRight, isLeft)
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..), oneof)
import qualified Data.Text as T

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
-- | Arbitrary semver
instance Arbitrary SemVer where
  arbitrary = semver' <$> arb <*> arb <*> arb <*> arbitrary where
    arb = abs <$> arbitrary

instance Arbitrary SemVerRange where
  arbitrary = oneof [Eq <$> arbitrary,
                     Lt <$> arbitrary,
                     Gt <$> arbitrary,
                     Leq <$> arbitrary,
                     Geq <$> arbitrary
                     ]

-- | Unsafe instance!
instance IsString SemVer where
  fromString s = case parseSemVer (T.pack s) of
    Right sv -> sv
    Left err -> error $ show err

-- | Unsafe instance!
instance IsString SemVerRange where
  fromString s = case parseSemVerRange (T.pack s) of
    Right svr -> svr
    Left err -> error $ show err

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary PrereleaseTag where
  arbitrary = oneof [IntTag . abs <$> arbitrary]

instance Arbitrary PrereleaseTags where
  arbitrary = PrereleaseTags <$> arbitrary

-- | Asserts that the first argument is a `Right` value equal to the second
-- argument.
shouldBeR :: (Show a, Show b, Eq b) => Either a b -> b -> IO ()
shouldBeR x y = do
  shouldSatisfy x isRight
  let Right x' = x
  x' `shouldBe` y

infixl 1 `shouldBeR`

shouldBeL :: (Show a, Show b, Eq a) => Either a b -> IO ()
shouldBeL x = shouldSatisfy x isLeft

main :: IO ()
main = hspec $ do
  describe "semver parsing" $ do
    it "should parse basic semvers" $ property $
      -- Pre-apply absolute value so we know they're positive integers
      \((abs -> maj, abs -> min, abs -> patch) :: (Int, Int, Int)) -> do
        let s = intercalate "." $ map tshow ([maj, min, patch] :: [Int])
        parseSemVer s `shouldBeR` semver maj min patch

    it "should parse a semver with only major version" $ property $
      \(abs -> maj :: Int) -> do
        parseSemVer (tshow maj) `shouldBeR` semver maj 0 0

    it "should parse a semver with only major and minor versions" $ property $
      \((abs -> maj, abs -> min) :: (Int, Int)) -> do
        let s = intercalate "." $ map tshow ([maj, min] :: [Int])
        parseSemVer s `shouldBeR` semver maj min 0

    it "pretty-printing is an injection" $ property $ \sv -> do
      parseSemVer (tshow sv) `shouldBeR` sv

    it "should parse a semver with metadata" $ do
      parseSemVer "1.2.3-pre+asdf" `shouldBeR` semver'' 1 2 3 ["pre"] ["asdf"]

    describe "with release tags" $ do
      it "should parse a semver with release tags" $ do
        parseSemVer "1.2.3-alpha" `shouldBeR` semver' 1 2 3 ["alpha"]
        parseSemVer "1.2.3alpha" `shouldBeR` semver' 1 2 3 ["alpha"]

      it "should parse a semver with multiple release tags" $ do
        parseSemVer "1.2.3-alpha.3" `shouldBeR` semver' 1 2 3 ["alpha", IntTag 3]
        parseSemVer "1.2.3alpha.3" `shouldBeR` semver' 1 2 3 ["alpha", IntTag 3]

  describe "prerelease tag comparison" $ do
    it "should treat empty lists as greater" $ property $
      \(tags::PrereleaseTags) -> case tags of
        PrereleaseTags [] -> return ()
        tags -> [] > tags `shouldBe` True

  describe "semver range parsing" $ do
    it "should parse a semver into an exact range" $ property $ \sv -> do
      -- This says that if we pretty-print a semver V and parse it as a
      -- semver range, we get the range "= V" back.
      parseSemVerRange (tshow sv) `shouldBeR` Eq sv

    it "pretty printing should be an injection" $ property $ \svr -> do
      -- This says that if we pretty-print a semver V and parse it as a
      -- semver range, we get the range "= V" back.
      parseSemVerRange (tshow svr) `shouldBeR` svr

    it "should parse a semver with partial version into a range" $ property $
      \(abs -> maj :: Int, abs -> min :: Int) -> do
        let expected = Geq (semver maj min 0) `And` Lt (semver maj (min + 1) 0)
            parseIt = parseSemVerRange . T.intercalate "."
        -- E.g. 1.2 =====> (>=1.2.0 <1.3)
        parseIt [tshow maj, tshow min] `shouldBeR` expected
        parseIt [tshow maj, tshow min, "X"] `shouldBeR` expected
        parseIt [tshow maj, tshow min, "x"] `shouldBeR` expected
        parseIt [tshow maj, tshow min, "*"] `shouldBeR` expected

    it "should parse a multi range" $ do
      parseSemVerRange "1.2.3-pre+asdf - 2.4.3-pre+asdf"
        `shouldBeR` Geq (semver'' 1 2 3 ["pre"] ["asdf"])
                     `And` Lt (semver'' 2 4 3 ["pre"] ["asdf"])

    it "should parse semvers with && instead of spaces" $ do
      let expected = Geq (semver 2 0 0) `And` Leq (semver 2 15 0)
      parseSemVerRange ">= 2 && <= 2.14" `shouldBeR` expected

    it "should fail when it's wrong" $ do
      shouldBeL (parseSemVerRange "xyz")

  rangeTests
  cleanTests

-- | These test cases were adapted from
-- https://github.com/npm/node-semver/blob/master/test/clean.js
cleanTests :: Spec
cleanTests = describe "unclean version strings" $ do
  let examples :: [(Text, Maybe Text)] = [
        ("1.2.3", Just "1.2.3"),
        (" 1.2.3 ", Just "1.2.3"),
        (" 1.2.3-4 ", Just "1.2.3-4"),
        (" 1.2.3-pre ", Just "1.2.3-pre"),
        ("  =v1.2.3   ", Just "1.2.3"),
        ("v1.2.3", Just "1.2.3"),
        (" v1.2.3 ", Just "1.2.3"),
        ("\t1.2.3", Just "1.2.3"),
        (">1.2.3", Nothing),
        ("~1.2.3", Nothing),
        ("<=1.2.3", Nothing)
        -- The example below is given in the tests but this doesn't
        -- seem like an error to me, so there.
        -- ("1.2.x", Nothing)
        ]
  forM_ examples $ \(string, result) -> case result of
    Just string' -> do
      it ("should parse " <> show string <> " same as " <> show string') $ do
        parseSemVer string `shouldSatisfy` isRight
        parseSemVer string `shouldBe` parseSemVer string'

    Nothing -> do
      it ("should not parse " <> show string) $ do
        parseSemVer string `shouldSatisfy` isLeft

-- | These test cases were adapted from
-- https://github.com/npm/node-semver/blob/master/test/index.js#L134
rangeTests :: Spec
rangeTests = describe "range tests" $ do
  -- In each case, the range described in the first element of the
  -- tuple should be satisfied by the concrete version described in
  -- the second element of the tuple.
  let testCases :: [(Text, Text)] = [
        ("1.0.0 - 2.0.0", "1.2.3"),
        ("^1.2.3+build", "1.2.3"),
        ("^1.2.3+build", "1.3.0"),
        ("1.2.3-pre+asdf - 2.4.3-pre+asdf", "1.2.3"),
        ("1.2.3-pre+asdf - 2.4.3pre+asdf", "1.2.3"),
        ("1.2.3pre+asdf - 2.4.3pre+asdf", "1.2.3"),
        ("1.2.3-pre+asdf - 2.4.3-pre+asdf", "1.2.3-pre.2"),
        ("1.2.3-pre+asdf - 2.4.3-pre+asdf", "2.4.3-alpha"),
        ("1.2.3+asdf - 2.4.3+asdf", "1.2.3"),
        ("1.0.0", "1.0.0"),
        (">=*", "0.2.4"),
        ("", "1.0.0"),
        ("*", "1.2.3"),
        ("*", "v1.2.3"),
        (">=1.0.0", "1.0.0"),
        (">=1.0.0", "1.0.1"),
        (">=1.0.0", "1.1.0"),
        (">1.0.0", "1.0.1"),
        (">1.0.0", "1.1.0"),
        ("<=2.0.0", "2.0.0"),
        ("<=2.0.0", "1.9999.9999"),
        ("<=2.0.0", "0.2.9"),
        ("<2.0.0", "1.9999.9999"),
        ("<2.0.0", "0.2.9"),
        (">= 1.0.0", "1.0.0"),
        (">=  1.0.0", "1.0.1"),
        (">=   1.0.0", "1.1.0"),
        ("> 1.0.0", "1.0.1"),
        (">  1.0.0", "1.1.0"),
        ("<=   2.0.0", "2.0.0"),
        ("<= 2.0.0", "1.9999.9999"),
        ("<=  2.0.0", "0.2.9"),
        ("<    2.0.0", "1.9999.9999"),
        ("<\t2.0.0", "0.2.9"),
        (">=0.1.97", "v0.1.97"),
        (">=0.1.97", "0.1.97"),
        ("0.1.20 || 1.2.4", "1.2.4"),
        (">=0.2.3 || <0.0.1", "0.0.0"),
        (">=0.2.3 || <0.0.1", "0.2.3"),
        (">=0.2.3 || <0.0.1", "0.2.4"),
        ("||", "1.3.4"),
        ("2.x.x", "2.1.3"),
        ("1.2.x", "1.2.3"),
        ("1.2.x || 2.x", "2.1.3"),
        ("1.2.x || 2.x", "1.2.3"),
        ("x", "1.2.3"),
        ("2.*.*", "2.1.3"),
        ("1.2.*", "1.2.3"),
        ("1.2.* || 2.*", "2.1.3"),
        ("1.2.* || 2.*", "1.2.3"),
        ("*", "1.2.3"),
        ("2", "2.1.2"),
        ("2.3", "2.3.1"),
        ("~2.4", "2.4.0"),
        ("~2.4", "2.4.5"),
        ("~>3.2.1", "3.2.2"),
        ("~1", "1.2.3"),
        ("~>1", "1.2.3"),
        ("~> 1", "1.2.3"),
        ("~1.0", "1.0.2"),
        ("~ 1.0", "1.0.2"),
        ("~ 1.0.3", "1.0.12"),
        (">=1", "1.0.0"),
        (">= 1", "1.0.0"),
        ("<1.2", "1.1.1"),
        ("< 1.2", "1.1.1"),
        ("~v0.5.4-pre", "0.5.5"),
        ("~v0.5.4-pre", "0.5.4"),
        ("=0.7.x", "0.7.2"),
        ("<=0.7.x", "0.7.2"),
        (">=0.7.x", "0.7.2"),
        ("<=0.7.x", "0.6.2"),
        ("~1.2.1 >=1.2.3", "1.2.3"),
        ("~1.2.1 =1.2.3", "1.2.3"),
        ("~1.2.1 1.2.3", "1.2.3"),
        ("~1.2.1 >=1.2.3 1.2.3", "1.2.3"),
        ("~1.2.1 1.2.3 >=1.2.3", "1.2.3"),
        ("~1.2.1 1.2.3", "1.2.3"),
        (">=1.2.1 1.2.3", "1.2.3"),
        ("1.2.3 >=1.2.1", "1.2.3"),
        (">=1.2.3 >=1.2.1", "1.2.3"),
        (">=1.2.1 >=1.2.3", "1.2.3"),
        (">=1.2", "1.2.8"),
        ("^1.2.3", "1.8.1"),
        ("^0.1.2", "0.1.2"),
        ("^0.1", "0.1.2"),
        ("^1.2", "1.4.2"),
        ("^1.2 ^1", "1.4.2"),
        ("^1.2.3-alpha", "1.2.3-pre"),
        ("^1.2.0-alpha", "1.2.0-pre"),
        ("^0.0.1-alpha", "0.0.1-beta")
        ]
  forM_ testCases $ \(range, version) -> do
    let fail_ = expectationFailure
    it (show version <> " satisfies range " <> show range) $ do
      case (parseSemVerRange range, parseSemVer version) of
        (Left err, _) -> fail $ "Semver range parse failed: " <> show err
        (_, Left err) -> fail $ "Semver parse failed: " <> show err
        (Right range, Right version) -> case matches range version of
          True -> True `shouldBe` True -- return ()
          False -> fail $ "Version " <> show version <> " didn't match range "
                       <> show range --`shouldBe` True
