{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Data.SemVer.Types where

import ClassyPrelude
import qualified Prelude as P
import Data.Text (Text)
import qualified Data.Text as T

type ReleaseTag = Text

-- | A SemVer has major, minor and patch versions, and zero or more
-- pre-release version tags.
data SemVer = SemVer {
  svMajor :: !Int,
  svMinor :: !Int,
  svPatch :: !Int,
  svReleaseTags :: ![ReleaseTag]
  } deriving (Eq, Ord)

instance Show SemVer where
  show (SemVer x y z []) = show x <> "." <> show y <> "." <> show z
  show (SemVer x y z tags) = show (semver x y z) <> "-" <>
                              (intercalate "." $ map unpack tags)

instance Hashable SemVer where
  hashWithSalt salt (SemVer major minor patch tags) =
    hashWithSalt salt (major, minor, patch, tags)

-- | A partially specified semantic version. Implicitly defines
-- a range of acceptable versions, as seen in @wildcardToRange@.
data Wildcard = Any
              | One Int
              | Two Int Int
              | Three Int Int Int [Text]
              deriving (Show, Eq)

-- | A range specifies bounds on a semver.
data SemVerRange
  = Eq SemVer                   -- ^ Exact equality
  | Gt SemVer                   -- ^ Greater than
  | Lt SemVer                   -- ^ Less than
  | Geq SemVer                  -- ^ Greater than or equal to
  | Leq SemVer                  -- ^ Less than or equal to
  | And SemVerRange SemVerRange -- ^ Conjunction
  | Or SemVerRange SemVerRange  -- ^ Disjunction
  deriving (Eq, Ord)

-- | Pull all of the concrete versions out of a range.
versionsOf :: SemVerRange -> [SemVer]
versionsOf = \case
  Eq sv -> [sv]
  Geq sv -> [sv]
  Leq sv -> [sv]
  Lt sv -> [sv]
  Gt sv -> [sv]
  And svr1 svr2 -> versionsOf svr1 <> versionsOf svr2
  Or svr1 svr2 -> versionsOf svr1 <> versionsOf svr2

-- | Create a SemVer with no version tags.
semver :: Int -> Int -> Int -> SemVer
semver a b c = SemVer a b c []

-- | Get only the version tuple from a semver.
toTuple :: SemVer -> (Int, Int, Int)
toTuple (SemVer a b c _) = (a, b, c)

-- | Get a list of tuples from a version range.
tuplesOf :: SemVerRange -> [(Int, Int, Int)]
tuplesOf = map toTuple . versionsOf

-- | Get all of the release tags from a version range.
rangeReleaseTags :: SemVerRange -> [ReleaseTag]
rangeReleaseTags = concatMap svReleaseTags . versionsOf

-- | Get the range release tags if they're all the same; otherwise
-- Nothing.
sharedReleaseTags :: SemVerRange -> Maybe [ReleaseTag]
sharedReleaseTags range = case map svReleaseTags $ versionsOf range of
  [] -> Nothing -- shouldn't happen but in case
  []:_ -> Nothing -- no release tags, so that seals it
  tagList:otherLists
    | all (== tagList) otherLists -> Just tagList
    | otherwise -> Nothing

-- | Satisfies any version.
anyVersion :: SemVerRange
anyVersion = Geq $ semver 0 0 0

-- | Render a semver as Text.
renderSV :: SemVer -> Text
renderSV = pack . show


instance Show SemVerRange where
  show = \case
    Eq sv -> "=" <> show sv
    Gt sv -> ">" <> show sv
    Lt sv -> "<" <> show sv
    Geq sv -> ">=" <> show sv
    Leq sv -> "<=" <> show sv
    And svr1 svr2 -> show svr1 <> " " <> show svr2
    Or svr1 svr2 -> show svr1 <> " || " <> show svr2

-- | Returns whether a given semantic version matches a range.
-- Note that there are special cases when there are release tags. For detauls
-- see https://github.com/npm/node-semver#prerelease-tags.
matches :: SemVerRange -> SemVer -> Bool
matches range version = case (sharedReleaseTags range, svReleaseTags version) of
  -- This is the simple case, where neither the range nor the version has given
  -- any release tags. Then we can just do regular predicate calculus.
  (Nothing, []) -> matchesSimple range version
  (Just rTags, vTags)
    | rTags == vTags -> matchesSimple range version
    | tuplesOf range /= [toTuple version] -> False
    | otherwise -> matchesTags range rTags vTags
  (_, _) -> False

-- | Simple predicate calculus matching, doing AND and OR combination with
-- numerical comparison.
matchesSimple :: SemVerRange -> SemVer -> Bool
matchesSimple range ver = case range of
  Eq sv -> ver == sv
  Gt sv -> ver > sv
  Lt sv -> ver < sv
  Geq sv -> ver >= sv
  Leq sv -> ver <= sv
  And sv1 sv2 -> matchesSimple sv1 ver && matchesSimple sv2 ver
  Or sv1 sv2 -> matchesSimple sv1 ver || matchesSimple sv2 ver

-- | Given a range and two sets of tags, the first being a bound on the second,
-- uses the range to compare the tags and see if they match.
matchesTags :: SemVerRange -> [ReleaseTag] -> [ReleaseTag] -> Bool
matchesTags range rangeTags verTags = case range of
  Eq _ -> verTags == rangeTags
  Gt _ -> verTags > rangeTags
  Lt _ -> verTags < rangeTags
  Geq _ -> verTags >= rangeTags
  Leq _ -> verTags <= rangeTags
  -- Note that as we're currently doing things, these cases won't get hit.
  And svr1 svr2 -> matchesTags svr1 verTags rangeTags &&
                     matchesTags svr2 verTags rangeTags
  Or svr1 svr2 -> matchesTags svr1 verTags rangeTags ||
                     matchesTags svr2 verTags rangeTags

-- | Gets the highest-matching semver in a range.
bestMatch :: SemVerRange -> [SemVer] -> Either String SemVer
bestMatch range vs = case filter (matches range) vs of
  [] -> Left "No matching versions"
  vs -> Right $ P.maximum vs

-- | Fills in zeros in a wildcard.
wildcardToSemver :: Wildcard -> SemVer
wildcardToSemver Any = semver 0 0 0
wildcardToSemver (One n) = semver n 0 0
wildcardToSemver (Two n m) = semver n m 0
wildcardToSemver (Three n m o tags) = SemVer n m o tags

-- | Translates a wildcard (partially specified version) to a range.
-- Ex: 2 := >=2.0.0 <3.0.0
-- Ex: 1.2.x := 1.2 := >=1.2.0 <1.3.0
wildcardToRange :: Wildcard -> SemVerRange
wildcardToRange = \case
  Any -> Geq $ semver 0 0 0
  One n -> Geq (semver n 0 0) `And` Lt (semver (n+1) 0 0)
  Two n m -> Geq (semver n m 0) `And` Lt (semver n (m+1) 0)
  Three n m o tags -> Eq (SemVer n m o tags)

-- | Translates a ~wildcard to a range.
-- Ex: ~1.2.3 := >=1.2.3 <1.(2+1).0 := >=1.2.3 <1.3.0
tildeToRange :: Wildcard -> SemVerRange
tildeToRange = \case
  -- I'm not sure this is officially supported, but just in case...
  Any -> tildeToRange (Three 0 0 0 [])
  -- ~1 := >=1.0.0 <(1+1).0.0 := >=1.0.0 <2.0.0 (Same as 1.x)
  One n -> Geq (semver n 0 0) `And` Lt (semver (n+1) 0 0)
  -- ~1.2 := >=1.2.0 <1.(2+1).0 := >=1.2.0 <1.3.0 (Same as 1.2.x)
  Two n m -> Geq (semver n m 0) `And` Lt (semver n (m+1) 0)
  -- ~1.2.3 := >=1.2.3 <1.(2+1).0 := >=1.2.3 <1.3.0
  Three n m o [] -> Geq (semver n m o) `And` Lt (semver n (m+1) 0)
  -- ~1.2.3-beta.2 := >=1.2.3-beta.2 <1.3.0
  Three n m o tags -> Geq (SemVer n m o tags) `And` Lt (semver n (m+1) 0)

-- | Translates a ^wildcard to a range.
-- Ex: ^1.2.x := >=1.2.0 <2.0.0
caratToRange :: Wildcard -> SemVerRange
caratToRange = \case
  One n -> Geq (semver n 0 0) `And` Lt (semver (n+1) 0 0)
  Two n m -> Geq (semver n m 0) `And` Lt (semver (n+1) 0 0)
  Three 0 0 n tags -> Eq (SemVer 0 0 n tags)
  Three 0 n m tags -> Geq (SemVer 0 n m tags) `And` Lt (SemVer 0 (n+1) 0 tags)
  Three n m o tags -> Geq (SemVer n m o tags) `And` Lt (SemVer (n+1) 0 0 tags)

-- | Translates two hyphenated wildcards to an actual range.
-- Ex: 1.2.3 - 2.3.4 := >=1.2.3 <=2.3.4
-- Ex: 1.2 - 2.3.4 := >=1.2.0 <=2.3.4
-- Ex: 1.2.3 - 2 := >=1.2.3 <3.0.0
hyphenatedRange :: Wildcard -> Wildcard -> SemVerRange
hyphenatedRange wc1 wc2 = And sv1 sv2 where
  sv1 = case wc1 of Any -> anyVersion
                    One n -> Geq (semver n 0 0)
                    Two n m -> Geq (semver n m 0)
                    Three n m o tags -> Geq (SemVer n m o tags)
  sv2 = case wc2 of Any -> anyVersion
                    One n -> Lt (semver (n+1) 0 0)
                    Two n m -> Lt (semver n (m+1) 0)
                    Three n m o tags -> Leq (SemVer n m o tags)
