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
  let testCases :: [(Bool, Text, Text)] = [

        -- Range constraints with pre-release tags require that any
        -- version satisfying the constraint must be equivalent (in
        -- its semantic version tuple) to the minimum of all semantic
        -- versions within the range. In this case the minimum of the
        -- range is "1.2.3" and the version's semantic version tuple
        -- is "1.2.4", therefore it does not satisfy the constraints
        -- of the range given the presence of pre-release tags.
        (False, "1.2.3-pre+asdf - 2.4.3-pre+asdf", "1.2.4-pre+asdf"),
        (False, "1.2.3-pre+asdf - 2.4.3-pre+asdf", "2.4.3-alpha"),
        (False, ">=0.0.1-alpha <0.2.0-alpha", "0.1.1-alpha"),
        (False, "^0.0.1-alpha", "0.0.4-alpha"),

        -- Range constraints without prerelease tags are very strict
        -- about not admitting versions *with* prerelease tags
        (False, "^0.1.2", "0.1.2-beta1"),
        (False, "^0.1.2", "0.1.4-beta1"),

        -- Despite the numeric quantity, these versions have
        -- prerelease tags and are therefore subjected to the same
        -- invariant checking.
        (False, "^1.2.3-1", "1.8.1-1"),
        (False, "^1.2.3-1", "1.8.1-4"),

        -- If we ever have an exact version tuple match at the top of
        -- a given range then it must satisfy the range constraint!
        --
        -- e.g. "^1.2.3-alpha" translates to ">=1.2.3-alpha
        -- <2.0.0-alpha" and the version to check is "2.0.0-alpha". In
        -- this case the version tuples are equivalent, sans
        -- prerelease tags, but it does not satisfy the upper-bound
        -- less-than relation.
        (False, "^1.2.3-alpha", "2.0.0-alpha"),

        (True,  "1.2.3-pre+asdf - 2.4.3-pre+asdf", "1.2.3-pre+asdf"),
        (True, "", "1.0.0"),
        (True, "*", "1.2.3"),
        (True, "*", "1.2.3"),
        (True, "*", "v1.2.3"),
        (True, "0.1.20 || 1.2.4", "1.2.4"),
        (True, "1.0.0 - 2.0.0", "1.2.3"),
        (True, "1.0.0", "1.0.0"),
        (True, "1.2.* || 2.*", "1.2.3"),
        (True, "1.2.* || 2.*", "2.1.3"),
        (True, "1.2.*", "1.2.3"),
        (True, "1.2.3 - 2.4.3", "1.2.4"),
        (True, "1.2.3 >=1.2.1", "1.2.3"),
        (True, "1.2.3+asdf - 2.4.3+asdf", "1.2.3"),
        (True, "1.2.3-pre+asdf - 2.4.3-pre+asdf", "1.2.3"),
        (True, "1.2.3-pre+asdf - 2.4.3-pre+asdf", "1.2.3-pre.2"),
        (True, "1.2.3-pre+asdf - 2.4.3-pre+asdf", "1.2.3-pred"),
        (True, "1.2.3-pre+asdf - 2.4.3pre+asdf", "1.2.3"),
        (True, "1.2.3pre+asdf - 2.4.3pre+asdf", "1.2.3"),
        (True, "1.2.x || 2.x", "1.2.3"),
        (True, "1.2.x || 2.x", "2.1.3"),
        (True, "1.2.x", "1.2.3"),
        (True, "2", "2.1.2"),
        (True, "2.*.*", "2.1.3"),
        (True, "2.3", "2.3.1"),
        (True, "2.x.x", "2.1.3"),
        (True, "<    2.0.0", "1.9999.9999"),
        (True, "< 1.2", "1.1.1"),
        (True, "<1.2", "1.1.1"),
        (True, "<2.0.0", "0.2.9"),
        (True, "<2.0.0", "1.9999.9999"),
        (True, "<=   2.0.0", "2.0.0"),
        (True, "<=  2.0.0", "0.2.9"),
        (True, "<= 2.0.0", "1.9999.9999"),
        (True, "<=0.7.x", "0.6.2"),
        (True, "<=0.7.x", "0.7.2"),
        (True, "<=2.0.0", "0.2.9"),
        (True, "<=2.0.0", "1.9999.9999"),
        (True, "<=2.0.0", "2.0.0"),
        (True, "<\t2.0.0", "0.2.9"),
        (True, "=0.7.x", "0.7.2"),
        (True, ">  1.0.0", "1.1.0"),
        (True, "> 1.0.0", "1.0.1"),
        (True, ">1.0.0", "1.0.1"),
        (True, ">1.0.0", "1.1.0"),
        (True, ">=   1.0.0", "1.1.0"),
        (True, ">=  1.0.0", "1.0.1"),
        (True, ">= 1", "1.0.0"),
        (True, ">= 1.0.0", "1.0.0"),
        (True, ">= 4.0.0 <4.1.0-0", "4.0.1"),
        (True, ">=*", "0.2.4"),
        (True, ">=0.1.97", "0.1.97"),
        (True, ">=0.1.97", "v0.1.97"),
        (True, ">=0.2.3 || <0.0.1", "0.0.0"),
        (True, ">=0.2.3 || <0.0.1", "0.2.3"),
        (True, ">=0.2.3 || <0.0.1", "0.2.4"),
        (True, ">=0.7.x", "0.7.2"),
        (True, ">=1", "1.0.0"),
        (True, ">=1.0.0", "1.0.0"),
        (True, ">=1.0.0", "1.0.1"),
        (True, ">=1.0.0", "1.1.0"),
        (True, ">=1.2", "1.2.8"),
        (True, ">=1.2.1 1.2.3", "1.2.3"),
        (True, ">=1.2.1 >=1.2.3", "1.2.3"),
        (True, ">=1.2.3 >=1.2.1", "1.2.3"),
        (True, "^0.0.1-alpha", "0.0.1-beta"),
        (True, "^0.0.1-alpha.1", "0.0.1-alpha.t"),
        (True, "^0.0.1-alpha.1", "0.0.1-alpha.tdff.dddddddddd"),
        (True, "^0.1", "0.1.2"),
        (True, "^0.1.2", "0.1.2"),
        (True, "^1.2 ^1", "1.4.2"),
        (True, "^1.2", "1.4.2"),
        (True, "^1.2.0-alpha", "1.2.0-pre"),
        (True, "^1.2.3", "1.8.1"),
        (True, "^1.2.3+build", "1.2.3"),
        (True, "^1.2.3+build", "1.3.0"),
        (True, "^1.2.3-alpha", "1.2.3-pre"),
        (True, "^1.2.3-alpha.1", "1.2.3-alpha.7"),
        (True, "^1.2.3-boop", "1.2.4"),
        (True, "x", "1.2.3"),
        (True, "||", "1.3.4"),
        (True, "~ 1.0", "1.0.2"),
        (True, "~ 1.0.3", "1.0.12"),
        (True, "~1", "1.2.3"),
        (True, "~1.0", "1.0.2"),
        (True, "~1.2.1 1.2.3 >=1.2.3", "1.2.3"),
        (True, "~1.2.1 1.2.3", "1.2.3"),
        (True, "~1.2.1 1.2.3", "1.2.3"),
        (True, "~1.2.1 =1.2.3", "1.2.3"),
        (True, "~1.2.1 >=1.2.3 1.2.3", "1.2.3"),
        (True, "~1.2.1 >=1.2.3", "1.2.3"),
        (True, "~2.4", "2.4.0"),
        (True, "~2.4", "2.4.5"),
        (True, "~> 1", "1.2.3"),
        (True, "~>1", "1.2.3"),
        (True, "~v0.5.4-pre", "0.5.4"),
        (True, "~v0.5.4-pre", "0.5.5"),
        (True, "~>3.2.1", "3.2.2")
        ]
  forM_ testCases $ \(expectedMatchBool, range, version) -> do
    let fail_ = expectationFailure
    it (show version <> " satisfies range " <> show range) $ do
      case (parseSemVerRange range, parseSemVer version) of
        (Left err, _) -> fail $ "Semver range parse failed: " <> show err
        (_, Left err) -> fail $ "Semver parse failed: " <> show err
        (Right range, Right version) -> matches range version `shouldBe` expectedMatchBool
