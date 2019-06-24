{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Data.SemVer.Types where

import ClassyPrelude
import qualified Prelude as P
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsList(..), Item)

-------------------------------------------------------------------------------
-- Prerelease tags

-- | Prerelease tags can either be numbers or text.
data PrereleaseTag
  = IntTag Int
  | TextTag Text
  deriving (Eq, Ord, Generic)

instance Show PrereleaseTag where
  show (IntTag i) = show i
  show (TextTag t) = T.unpack t

instance IsString PrereleaseTag where
  fromString = TextTag . fromString

instance Hashable PrereleaseTag

newtype PrereleaseTags = PrereleaseTags [PrereleaseTag]
#if MIN_VERSION_base(4,11,0)
  deriving (Show, Eq, Semigroup, Monoid, Generic)
#else
  deriving (Show, Eq, Monoid, Generic)
#endif

instance IsList PrereleaseTags where
  type Item PrereleaseTags = PrereleaseTag
  fromList = PrereleaseTags
  toList (PrereleaseTags tags) = tags

instance Hashable PrereleaseTags
instance Ord PrereleaseTags where
  -- | Compare two lists of prerelease tags. See for reference:
  --
  -- https://github.com/npm/node-semver/blob/
  --   d21444a0658224b152ce54965d02dbe0856afb84/semver.js#L356
  --
  -- Note that having no prerelease tags is considered "greater" than having
  -- them, the idea being that prerelease tags indicate a version which
  -- is not yet complete. Conversely, if neither is empty, then greater length
  -- is considered to be "greater" overall, if two versions have the same
  -- prefix.
  --
  -- Examples:
  --   [A, B] < []
  --   [1, 2, 3] < [2]
  --   [1, 2] < [1, 2, 3]
  compare (PrereleaseTags prt1) (PrereleaseTags prt2) = case (prt1, prt2) of
    ([], _:_) -> GT
    (_:_, []) -> GT
    _ -> go $ zipMaybe prt1 prt2 where
      zipMaybe (x:xs) (y:ys)  =  (Just x, Just y) : zipMaybe xs ys
      zipMaybe xs     []      =  [(Just x, Nothing) | x <- xs]
      zipMaybe []     ys      =  [(Nothing, Just y) | y <- ys]

      go [] = EQ -- They were the same
      go ((Nothing, Nothing):_) = EQ -- Same as above (shouldn't happen but)
      go ((Just _, Nothing):_) = GT -- First list was longer than the second.
      go ((Nothing, Just _):_) = LT -- Second list was longer than the first.
      go ((Just tag1, Just tag2):rest) = case compare tag1 tag2 of
        EQ -> go rest
        result -> result

-------------------------------------------------------------------------------
-- Build Metadata
--
-- Extra data that can be attached to a version, but which doesn't affect its
-- version comparison.
type BuildMetaData = [Text]

-------------------------------------------------------------------------------
-- Semantic versions (SemVers)
--
-- | A SemVer has major, minor and patch versions, and zero or more
-- pre-release version tags.
data SemVer = SemVer {
  svMajor :: !Int,
  svMinor :: !Int,
  svPatch :: !Int,
  svTags :: !PrereleaseTags,
  svBuildMetadata :: !BuildMetaData
  } deriving (Eq, Generic)

-- | Define an Ord instance which ignores the buildMetaData.
instance Ord SemVer where
  compare (SemVer maj1 min1 pat1 tags1 _) (SemVer maj2 min2 pat2 tags2 _) =
    compare (maj1, min1, pat1, tags1) (maj2, min2, pat2, tags2)

instance Show SemVer where
  show (SemVer x y z tags mdata) = base <> tags' <> mdata' where
    base = show x <> "." <> show y <> "." <> show z
    tags' = case tags of
      PrereleaseTags [] -> mempty
      PrereleaseTags tags -> "-" <> intercalate "." (map show tags)
    mdata' = case mdata of
      [] -> mempty
      stuff -> "+" <> intercalate "." (map T.unpack stuff)

instance Hashable SemVer

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

infixl 3 `And`
infixl 3 `Or`
infixl 4 `Eq`
infixl 4 `Gt`
infixl 4 `Geq`
infixl 4 `Lt`
infixl 4 `Leq`

instance Show SemVerRange where
  show = \case
    Eq sv -> "=" <> show sv
    Gt sv -> ">" <> show sv
    Lt sv -> "<" <> show sv
    Geq sv -> ">=" <> show sv
    Leq sv -> "<=" <> show sv
    And svr1 svr2 -> show svr1 <> " " <> show svr2
    Or svr1 svr2 -> show svr1 <> " || " <> show svr2

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

-- | Strip out all prerelease tags from a given 'SemVerRange'.
stripRangeTags :: SemVerRange -> SemVerRange
stripRangeTags = \case
  Eq  sv -> Eq  (sv { svTags = [] })
  Geq sv -> Geq (sv { svTags = [] })
  Leq sv -> Leq (sv { svTags = [] })
  Lt  sv -> Lt  (sv { svTags = [] })
  Gt  sv -> Gt  (sv { svTags = [] })
  And svr1 svr2 -> And (stripRangeTags svr1) (stripRangeTags svr2)
  Or  svr1 svr2 -> Or  (stripRangeTags svr1) (stripRangeTags svr2)

-- | Create a SemVer with no version tags.
semver :: Int -> Int -> Int -> SemVer
semver major minor patch = semver' major minor patch []

-- | Create a SemVer with tags
semver' :: Int -> Int -> Int -> PrereleaseTags -> SemVer
semver' major minor patch tags = semver'' major minor patch tags []

-- | Create a SemVer with tags and metadata.
semver'' :: Int -> Int -> Int -> PrereleaseTags -> BuildMetaData -> SemVer
semver'' = SemVer

-- | Get only the version tuple from a semver.
toTuple :: SemVer -> (Int, Int, Int)
toTuple (SemVer a b c _ _) = (a, b, c)

-- | Get a list of tuples from a version range.
tuplesOf :: SemVerRange -> [(Int, Int, Int)]
tuplesOf = map toTuple . versionsOf

-- | Get all of the prerelease tags from a version range.
rangePrereleaseTags :: SemVerRange -> PrereleaseTags
rangePrereleaseTags = concatMap svTags . versionsOf

-- | Get the range prerelease tags if they're all the same; otherwise
-- Nothing.
sharedTags :: SemVerRange -> Maybe PrereleaseTags
sharedTags range = case map svTags $ versionsOf range of
  [] -> Nothing -- shouldn't happen but in case
  []:_ -> Nothing -- no prerelease tags, so that seals it
  tagList:otherLists
    | all (== tagList) otherLists -> Just tagList
    | otherwise -> Nothing

-- | Satisfies any version.
anyVersion :: SemVerRange
anyVersion = Geq $ semver 0 0 0

-- | Render a semver as Text.
renderSV :: SemVer -> Text
renderSV = pack . show

-- | Returns whether a given semantic version matches a range.
-- Note that there are special cases when there are prerelease tags. For
-- details see https://github.com/npm/node-semver#prerelease-tags.
matches :: SemVerRange -> SemVer -> Bool
matches range version =
  case (sharedTags range, svTags version) of

    (Nothing, PrereleaseTags vTags)
      -- Neither the range nor the version have prerelease tags
      | null vTags -> matchesSimple range version

      -- If there is no prerelease tag in the range but there is in
      -- the version reject it
      | otherwise  -> False

    -- A range with a prerelease tag can match a version without a
    -- prerelease tag provided it *does* meet the semantic version
    -- tuple's constraint criteria
    (Just _, PrereleaseTags []) ->
      matchesSimple range version

    -- The most important invariant when considering a comparison
    -- between a range with prerelease tags and a version with
    -- prerelease tags is whether the semantic version in both is the
    -- same; if it is not, then we must reject the version.
    --
    -- Note that we could have a conjunction or a disjunction, so we
    -- want to see if our version tuple is in the list of tuples for
    -- the range. However, it would be possible to then match with,
    -- say, the upper-bound version tuple which may be constrained by
    -- a less-than relation. Therefore, if there is an equivalent
    -- range tuple to the version tuple, we want to check if it
    -- satisfies the constraints with the goal of rejecting early.
    --
    -- For example, if we assume a range constraint of "^1.2.3-alpha"
    -- this translates to ">=1.2.3-alpha <2.0.0-alpha". Also assume we
    -- have the version "1.2.3-alpha". In the trivial case, we check
    -- to see if the version's tuple ("1.2.3") is in the set of
    -- version tuples for the range ([ (1.2.3), (2.0.0) ]). We can
    -- clearly see that it is, therefore we proceed with a match check
    -- on the tags.
    --
    -- However, consider matching "2.0.0-alpha" against the range
    -- constraint we've already given. If we only check for membership
    -- of our version tuple ("2.0.0") in the set of range tuples ([
    -- (1.2.3), (2.0.0) ]) then we would get a match, this is not
    -- correct. Thus, if the version tuple is a member of the set of
    -- range tuples we must also check that it satisfies the range
    -- constraints sans prerelease tags.
    (Just rTags, vTags)

      -- Explicit rejection, e.g. "^1.2.3-alpha" must reject
      -- "1.2.4-alpha" and "2.0.0-alpha", anything else is safe to
      -- compare based on tags so we can let it "fall through".
      | versionTuple `notElem` rangeTuple || not (matchesSimple rangeNoTags versionNoTags)
        -> False

      | rTags == vTags
        -> True

      | rTags /= vTags
        -> matchesTags range rTags vTags

  where
    rangeTuple   = tuplesOf range
    versionTuple = toTuple version

    rangeNoTags   = stripRangeTags range
    versionNoTags = version { svTags = [] }

-- | Simple predicate calculus matching, doing AND and OR combination with
-- numerical comparison.
matchesSimple :: SemVerRange -> SemVer -> Bool
matchesSimple range ver = case range of
  Eq sv -> ver == sv
  Gt sv -> ver > sv
  Lt sv -> ver < sv
  Geq sv -> ver >= sv
  Leq sv -> ver <= sv
  And range1 range2 -> matches range1 ver && matches range2 ver
  Or range1 range2 -> matches range1 ver || matches range2 ver

infixl 2 `matches`

-- | Given a range and two sets of tags, the first being a bound on the second,
-- uses the range to compare the tags and see if they match.
matchesTags :: SemVerRange -> PrereleaseTags -> PrereleaseTags -> Bool
matchesTags range rangeTags verTags =
  case range of
    Eq _  -> verTags == rangeTags
    Gt _  -> verTags >  rangeTags
    Lt _  -> verTags <  rangeTags
    Geq _ -> verTags >= rangeTags
    Leq _ -> verTags <= rangeTags

    And svr1 svr2 ->
      matchesTags svr1 rangeTags verTags
    Or svr1 svr2 ->
      matchesTags svr1 rangeTags verTags || matchesTags svr2 rangeTags verTags

-- | Gets the highest-matching semver in a range.
bestMatch :: SemVerRange -> [SemVer] -> Either String SemVer
bestMatch range vs = case filter (matches range) vs of
  [] -> Left "No matching versions"
  vs -> Right $ P.maximum vs
