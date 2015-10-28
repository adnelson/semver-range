{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Data.SemVer.Parser (
    parseSemVer, parseSemVerRange, pSemVerRange, pSemVer,
    fromHaskellVersion, matchText
  ) where

import qualified Prelude as P
import ClassyPrelude hiding (try)
import Text.Parsec hiding ((<|>), spaces, parse, State, uncons)
import qualified Text.Parsec as Parsec

import Data.Version (Version(..))
import Data.SemVer

type Parser = ParsecT String () Identity

-- | Given a parser and a string, attempts to parse the string.
parse :: Parser a -> Text -> Either ParseError a
parse p = Parsec.parse p "" . unpack

parseFull :: Parser a -> Text -> Either ParseError a
parseFull p = Parsec.parse (p <* eof) "" . unpack

-- | Consumes any spaces (not other whitespace).
spaces :: Parser String
spaces = many $ char ' '

-- | Consumes at least one space (not other whitespace).
spaces1 :: Parser String
spaces1 = many1 $ char ' '

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
parseSemVerRange = parse pSemVerRange

-- | Parse a string as an explicit version, or return an error.
parseSemVer :: Text -> Either ParseError SemVer
parseSemVer = parse pSemVer

-- | Parses a semantic version.
pSemVer :: Parser SemVer
pSemVer = wildcardToSemver <$> pWildCard

pVersionComp :: Parser SemVerRange
pVersionComp = do
  comparator <- cmp
  ver <- pSemVer
  let func = case comparator of {"=" -> Eq; ">" -> Gt; "<" -> Lt;
                                 ">=" -> Geq; "<=" -> Leq; "==" -> Eq}
  return $ func ver

-- | Parses a comparison operator.
cmp :: Parser String
cmp = choice $ fmap (try . sstring) [">=", "<=", ">", "<", "==", "="]

-- | Parses versions with an explicit range qualifier (gt, lt, etc).
pSemVerRangeSingle :: Parser SemVerRange
pSemVerRangeSingle = choice [
    wildcardToRange <$> pWildCard,
    tildeToRange <$> pTildeRange,
    caratToRange <$> pCaratRange,
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
  let stripNothings [Nothing] = []
      stripNothings (Just x:xs) = x : stripNothings xs
      tag = fmap pack $ many1 $ letter <|> digit <|> char '-'
  -- Versions can optionally start with the character 'v'
  optional (char 'v')
  res <- takeWhile isJust <$> sepBy1 bound (sstring ".") >>= \case
    [] -> return Any
    [Just n] -> return $ One n
    [Just n, Just m] -> return $ Two n m
    [Just n, Just m, Just o] -> option (Three n m o []) $ do
      char '-'
      tags <- tag `sepBy1` char '.'
      return $ Three n m o tags
    w -> unexpected ("Invalid version " ++ show w)
  spaces *> return res

-- | Parses a tilde range (~1.2.3).
pTildeRange :: Parser Wildcard
pTildeRange = do
  sstring "~"
  -- For some reason, including the following operators after
  -- a tilde is valid, but seems to have no effect.
  optional $ choice [try $ sstring ">=", sstring ">", sstring "="]
  pWildCard

-- | Parses a carat range (^1.2.3).
pCaratRange :: Parser Wildcard
pCaratRange = sstring "^" *> pWildCard

-- | Top-level parser. Parses a semantic version range.
pSemVerRange :: Parser SemVerRange
pSemVerRange = try pHyphen <|> pJoinedSemVerRange


-- | Parse a semver from a haskell version. There must be exactly
-- three numbers in the versionBranch field.
fromHaskellVersion :: Version -> Either Text SemVer
fromHaskellVersion v = case versionBranch v of
  [x, y, z] -> return (x, y, z, []) -- ignoring version tags since deprecated
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
