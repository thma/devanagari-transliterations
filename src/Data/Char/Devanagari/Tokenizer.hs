module Data.Char.Devanagari.Tokenizer
  ( selectTokenizerByContent,
    tokenize,
    fromDevanagari,
    fromIso,
    fromHarvard,
    fromIast,
    Tokenizer,
  )
where

{-
  This module contains Tokenizers for the Devanagari script and its roman transliterations Harvard-Kyoto, IAST and ISO15919.
  It also contains a generic tokenize function that detects the input encoding and automatically selects the right
  tokenizer to use.
-}

import           Control.Monad                         (join)
import           Data.Char.Devanagari.DevanagariTokens
import           Data.Char.Devanagari.TokenTables
import           Data.Map.Strict                       (Map)
import qualified Data.Map.Strict                       as Map
import           Data.Maybe                            (isJust)
import           Data.Sequence                         (Seq (Empty, (:<|)),
                                                        empty, (|>))
import           Data.Text                             (Text)
import qualified Data.Text                       as T

-- | A Tokenizer is a function that takes a Text as input and produces a Sequence of DevanagariToken instances as output.
type Tokenizer = (Text -> Seq DevanagariToken)

-- | A ParseMap is a Map from a Text to a DevanagariToken.
type ParseMap = Map Text DevanagariToken

harvardKyotoParseMap :: ParseMap
harvardKyotoParseMap = Map.fromList (harvardKyotoTable ++ inputVirams)

iastParseMap :: ParseMap
iastParseMap = Map.fromList (iastTable ++ inputVirams)

isoParseMap :: ParseMap
isoParseMap = Map.fromList (isoTable ++ inputVirams)

devanagariParseMap :: ParseMap
devanagariParseMap = Map.fromList (devanagariIndependentTable ++ devanagariDependentTable ++ inputVirams)

-- | parse a Text into a Sequence of DevanagariToken instances using a ParseMap.
parse :: ParseMap -> Text -> Seq DevanagariToken
parse pMap s = parse1 s pMap empty
  where
    parse1 :: Text -> ParseMap -> Seq DevanagariToken -> Seq DevanagariToken
    parse1 str _ tokens
      | str == T.empty = tokens
    parse1 str parseMap tokens =
      case tryMatch str 3 parseMap of
        Just (token, rest) -> parse1 rest parseMap (tokens |> token)
        Nothing ->
          case tryMatch str 2 parseMap of
            Just (token, rest) -> parse1 rest parseMap (tokens |> token)
            Nothing ->
              case tryMatch str 1 parseMap of
                Just (token, rest) -> parse1 rest parseMap (tokens |> token)
                Nothing -> parse1 (T.drop 1 str) parseMap (tokens |> Unmapped (head $ T.unpack $ T.take 1 str))

    tryMatch :: Text -> Int -> ParseMap -> Maybe (DevanagariToken, Text)
    tryMatch str n parseMap =
      let tok = T.take n str
          rest = snd $ T.splitAt (T.length tok) str
          maybeToken = Map.lookup tok parseMap
       in case maybeToken of
            Just token -> Just (token, rest)
            Nothing    -> Nothing

-- | a tokenizer function that parses a Text containing IAST encoded Devanagari script into a Sequence of DevanagariToken instances.
fromIast :: Tokenizer
fromIast = parse iastParseMap

-- | a tokenizer function that parses a Text containing ISO15919 encoded Devanagari script into a Sequence of DevanagariToken instances.
fromIso :: Tokenizer
fromIso = parse isoParseMap

-- | a tokenizer function that parses a Text containing Harvard-Kyoto encoded Devanagari script into a Sequence of DevanagariToken instances.
fromHarvard :: Tokenizer
fromHarvard = parse harvardKyotoParseMap

-- | a tokenizer function that parses a Text containing Devanagari script into a Sequence of DevanagariToken instances.
fromDevanagari :: Tokenizer
fromDevanagari s = addExplicitVowA empty (parse devanagariParseMap s)
  where
    addExplicitVowA :: Seq DevanagariToken -> Seq DevanagariToken -> Seq DevanagariToken
    addExplicitVowA acc Empty = acc
    addExplicitVowA acc (cons@(Cons _) :<| Virama :<| xs) = addExplicitVowA (acc |> cons) xs
    addExplicitVowA acc (cons@(Cons _) :<| vow@(Vow _) :<| xs) = addExplicitVowA (acc |> cons |> vow) xs
    addExplicitVowA acc (cons@(Cons _) :<| xs) = addExplicitVowA (acc |> cons |> Vow A) xs
    addExplicitVowA acc (x :<| xs) = addExplicitVowA (acc |> x) xs

-- | tokenize a string of Text into a sequence of DevanagariTokens.
-- The actual tokenizer is selected based on the content of the input string.
-- This tokenizer is then applied to the input string.
tokenize :: Tokenizer
tokenize = join selectTokenizerByContent

-- | select the correct tokenizer based on the content of the input string.
selectTokenizerByContent :: Text -> Tokenizer
selectTokenizerByContent str
  | containsDevanagari str = fromDevanagari
  | containsIso str = fromIso
  | containsIast str = fromIast
  | otherwise = fromHarvard
  where
    containsDevanagari = containsAnyOf (['\x900' .. '\x963'] ++ ['\x966' .. '\x97F']) -- Unicode section for Devanagari
    containsIso = containsAnyOf ("ēōṁ" ++ ['\0325', '\0304']) -- ISO15919 diacritics
    containsIast = containsAnyOf (['\241' .. '\363'] ++ ['\7693' .. '\7789']) -- IAST diacritics
    containsAnyOf :: [Char] -> Text -> Bool
    containsAnyOf chars text = T.any (`isInfixOf` text) $ T.pack chars
    isInfixOf c text = isJust (T.find (== c) text)
