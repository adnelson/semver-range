{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Data.SemVer.Parser where -- (
  --   parseSemVer, parseSemVerRange, pSemVerRange, pSemVer, p
  --   fromHaskellVersion, matchText
  -- ) where

import qualified Prelude as P
import ClassyPrelude hiding (try, many)
import Text.Parsec hiding ((<|>), spaces, parse, State, uncons, optional)
import qualified Text.Parsec as Parsec
import qualified Data.Text as T
import Text.Read (readMaybe)

import Data.Version (Version(..))
import Data.SemVer.Types

type Parser = ParsecT String () Identity

-------------------------------------------------------------------------------
-- Wildcards: intermediate representations of semvers
--
-- | A partially specified semantic version. Implicitly defines
-- a range of acceptable versions, as seen in @wildcardToRange@.
data Wildcard = Any
              | One Int
              | Two Int Int
              | Full SemVer
              deriving (Show, Eq)

-- | Fills in zeros in a wildcard.
wildcardToSemver :: Wildcard -> SemVer
wildcardToSemver Any = semver 0 0 0
wildcardToSemver (One n) = semver n 0 0
wildcardToSemver (Two n m) = semver n m 0
wildcardToSemver (Full sv) = sv

-- | Translates a wildcard (partially specified version) to a range.
-- Ex: 2 := >=2.0.0 <3.0.0
-- Ex: 1.2.x := 1.2 := >=1.2.0 <1.3.0
wildcardToRange :: Wildcard -> SemVerRange
wildcardToRange = \case
  Any -> Geq $ semver 0 0 0
  One n -> Geq (semver n 0 0) `And` Lt (semver (n+1) 0 0)
  Two n m -> Geq (semver n m 0) `And` Lt (semver n (m+1) 0)
  Full sv -> Eq sv

-- | Translates a ~wildcard to a range.
-- Ex: ~1.2.3 := >=1.2.3 <1.(2+1).0 := >=1.2.3 <1.3.0
tildeToRange :: Wildcard -> SemVerRange
tildeToRange = \case
  -- I'm not sure this is officially supported, but just in case...
  Any -> tildeToRange (Full $ semver 0 0 0)
  -- ~1 := >=1.0.0 <(1+1).0.0 := >=1.0.0 <2.0.0 (Same as 1.x)
  One n -> Geq (semver n 0 0) `And` Lt (semver (n+1) 0 0)
  -- ~1.2 := >=1.2.0 <1.(2+1).0 := >=1.2.0 <1.3.0 (Same as 1.2.x)
  Two n m -> Geq (semver n m 0) `And` Lt (semver n (m+1) 0)
  -- ~1.2.3 := >=1.2.3 <1.(2+1).0 := >=1.2.3 <1.3.0
  Full (SemVer n m o [] _) -> Geq (semver n m o) `And` Lt (semver n (m+1) 0)
  -- ~1.2.3-beta.2 := >=1.2.3-beta.2 <1.3.0
  Full (SemVer n m o tags _) -> Geq (semver' n m o tags) `And` Lt (semver n (m+1) 0)

-- | Translates a ^wildcard to a range.
-- Ex: ^1.2.x := >=1.2.0 <2.0.0
caratToRange :: Wildcard -> SemVerRange
caratToRange = \case
  One n -> Geq (semver n 0 0) `And` Lt (semver (n+1) 0 0)
  Two n m -> Geq (semver n m 0) `And` Lt (semver (n+1) 0 0)
  Full (SemVer 0 n m tags _) -> Geq (semver' 0 n m tags) `And` Lt (semver' 0 (n+1) 0 tags)
  Full (SemVer n m o tags _) -> Geq (semver' n m o tags) `And` Lt (semver' (n+1) 0 0 tags)

-- | Translates two hyphenated wildcards to an actual range.
-- Ex: 1.2.3 - 2.3.4 := >=1.2.3 <=2.3.4
-- Ex: 1.2 - 2.3.4 := >=1.2.0 <=2.3.4
-- Ex: 1.2.3 - 2 := >=1.2.3 <3.0.0
hyphenatedRange :: Wildcard -> Wildcard -> SemVerRange
hyphenatedRange wc1 wc2 = And sv1 sv2 where
  sv1 = case wc1 of Any -> anyVersion
                    One n -> Geq (semver n 0 0)
                    Two n m -> Geq (semver n m 0)
                    Full sv -> Geq sv
  sv2 = case wc2 of Any -> anyVersion
                    One n -> Lt (semver (n+1) 0 0)
                    Two n m -> Lt (semver n (m+1) 0)
                    Full sv -> Lt sv

-- | Given a parser and a string, attempts to parse the string.
parse :: Parser a -> Text -> Either ParseError a
parse p = Parsec.parse p "" . unpack

parseFull :: Parser a -> Text -> Either ParseError a
parseFull p = Parsec.parse (p <* eof) "" . unpack

-- | Consumes any spaces (not other whitespace).
spaces :: Parser String
spaces = many $ oneOf [' ', '\t']

-- | Consumes at least one space (not other whitespace).
spaces1 :: Parser String
spaces1 = many1 $ oneOf [' ', '\t']

-- | Parses the given string and any trailing spaces.
sstring :: String -> Parser String
sstring = lexeme . string

-- | Parses the given character and any trailing spaces.
schar :: Char -> Parser Char
schar = lexeme . char

-- | Parses `p` and any trailing spaces.
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Parses an integer.
pInt :: Parser Int
pInt = lexeme pInt'

-- | Parses an integer without consuming trailing spaces.
pInt' :: Parser Int
pInt' = P.read <$> many1 digit

-- | Parse a string as a version range, or return an error.
parseSemVerRange :: Text -> Either ParseError SemVerRange
parseSemVerRange text = case T.strip text of
  -- Handle a few special cases
  "" -> return anyVersion
  "||" -> return anyVersion
  t -> parse pSemVerRange t

-- | Parse a string as an explicit version, or return an error.
parseSemVer :: Text -> Either ParseError SemVer
parseSemVer = parse pSemVer

-- | Parses a semantic version.
pSemVer :: Parser SemVer
pSemVer = wildcardToSemver <$> pWildCard

pVersionComp :: Parser SemVerRange
pVersionComp = cmp >>= \case
  "=" -> wildcardToRange <$> pWildCard
  "==" -> wildcardToRange <$> pWildCard
  -- This is a special case to deal with a test case in the npm semver
  -- test suite. The case states that "0.7.2" should satisfy
  -- "<=0.7.x". I'm interpreting this to mean that "<= X", where X is
  -- a range, means "less than or equal to the maximum supported in
  -- this range."
  "<=" -> Leq . topOf <$> pWildCard
  ">=" -> Geq <$> pSemVer
  ">" -> Gt <$> pSemVer
  "<" -> Lt <$> pSemVer
  where
    topOf = \case
      Any -> semver 0 0 0
      One n -> semver (n+1) 0 0
      Two n m -> semver n (m+1) 0
      Full sv -> sv

-- | Parses a comparison operator.
cmp :: Parser String
cmp = choice (try . sstring <$> [">=", "<=", ">", "<", "==", "="])

-- | Parses versions with an explicit range qualifier (gt, lt, etc).
pSemVerRangeSingle :: Parser SemVerRange
pSemVerRangeSingle = choice [
    wildcardToRange <$> pWildCard,
    pTildeRange,
    pCaratRange,
    pVersionComp
  ]

-- | Parses semantic version ranges joined with Ands and Ors.
pJoinedSemVerRange :: Parser SemVerRange
pJoinedSemVerRange = do
  first <- pSemVerRangeSingle
  option first $ do
    lookAhead (sstring "||" <|> cmp) >>= \case
      "||" -> Or first <$> (sstring "||" *> pJoinedSemVerRange)
      _ -> And first <$> pJoinedSemVerRange

-- | Parses a hyphenated range.
pHyphen :: Parser SemVerRange
pHyphen = hyphenatedRange <$> pWildCard <*> (sstring "-" *> pWildCard)

-- | Parses a "wildcard" (which is a possibly partial semantic version).
pWildCard :: Parser Wildcard
pWildCard = try $ do
  let seps = choice $ map string ["x", "X", "*"]
  let bound = choice [seps *> pure Nothing, Just <$> pInt']
  let getTag t = case readMaybe t of
        Just i -> IntTag i
        _ -> TextTag $ pack t
  let tag = getTag <$> many1 (letter <|> digit <|> char '-')
  -- Versions can optionally start with the character 'v'; ignore this.
  optional (char 'v')
  res <- takeWhile isJust <$> sepBy1 bound (sstring ".") >>= \case
    [] -> return Any
    [Just n] -> return $ One n
    [Just n, Just m] -> return $ Two n m
    [Just n, Just m, Just o] -> option (Full $ semver n m o) $ do
      -- Release tags might be separated by a hyphen, or not.
      optional (char '-')
      tags <- PrereleaseTags <$> (tag `sepBy1` char '.')
      -- Grab metadata if there is any
      option (Full $ semver'' n m o tags []) $ do
        char '+'
        metadata <- many1 (letter <|> digit <|> char '-') `sepBy1` char '.'
        return $ Full $ semver'' n m o tags (map pack metadata)
    w -> unexpected ("Invalid version " ++ show w)
  spaces *> return res

-- | Parses a tilde range (~1.2.3).
pTildeRange :: Parser SemVerRange
pTildeRange = do
  sstring "~"
  -- For some reason, including the following operators after
  -- a tilde is valid, but seems to have no effect.
  optional $ choice [try $ sstring ">=", sstring ">", sstring "="]
  tildeToRange <$> pWildCard

-- | Parses a carat range (^1.2.3).
pCaratRange :: Parser SemVerRange
pCaratRange = sstring "^" *> map caratToRange pWildCard

-- | Top-level parser. Parses a semantic version range.
pSemVerRange :: Parser SemVerRange
pSemVerRange = try pHyphen <|> pJoinedSemVerRange

-- | Parse a semver from a haskell version. There must be exactly
-- three numbers in the versionBranch field.
fromHaskellVersion :: Version -> Either Text SemVer
fromHaskellVersion v = case versionBranch v of
  [x, y, z] -> return (semver x y z) -- ignoring version tags since deprecated
  bad -> do
    let badVer = intercalate "." (map show bad)
    Left $ pack ("Not a SemVer version: " <> badVer)

-- | Parses the first argument as a range and the second argument as a semver,
-- and returns whether they match.
matchText :: Text -> Text -> Either Text Bool
matchText rangeTxt verTxt = case parseSemVerRange rangeTxt of
  Left err -> Left ("Could not parse range: " <> pack (show err))
  Right range -> case parseSemVer verTxt of
    Left err -> Left ("Could not parse version: " <> pack (show err))
    Right version -> Right $ matches range version
