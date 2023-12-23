module Data.Char.Devanagari.Generators
  ( toIast,
    toDevanagari,
    toIso,
    toHarvard,
    tokenMapToMd,
    tokenMapToHtml,
    tokenMap,
  )
where

{-
  This module contains Generators for the Devanagari script and its roman transliterations Harvard-Kyoto, IAST and ISO15919.
-}

import           Data.Char.Devanagari.DevanagariTokens
import           Data.Char.Devanagari.TokenTables
import           Data.List.Extra                       (enumerate)
import           Data.Map.Strict                       (Map)
import qualified Data.Map.Strict                       as Map
import           Data.Maybe                            (fromJust)
import           Data.Sequence
import qualified Data.Text.Short                       as TS
import           Data.Tuple                            (swap)

-- | a TranslateMap models a specific transliteration scheme from DevanagariTokens to a given representation
-- (e.g. Harvard-Kyoto, IAST, ISO15919, Devanagari)
type TranslateMap = Map DevanagariToken TS.ShortText

-- | a Generator is a function that takes a sequence of DevanagariTokens and returns a specific textual representation of the sequence
-- (e.g. Harvard-Kyoto, IAST, ISO15919, Devanagari)
type Generator = Seq DevanagariToken -> TS.ShortText

independentMapDevanagari :: TranslateMap
independentMapDevanagari = Map.fromList (map swap $ devanagariIndependentTable ++ viramsDeva)

dependentMapDevanagari :: TranslateMap
dependentMapDevanagari = Map.fromList (map swap $ devanagariDependentTable ++ viramsDeva)

iastMap :: TranslateMap
iastMap = Map.fromList (map swap $ iastTable ++ virams)

isoMap :: TranslateMap
isoMap = Map.fromList (map swap $ isoTable ++ virams)

harvardMap :: TranslateMap
harvardMap = Map.fromList (map swap $ harvardKyotoTable ++ viramsHarvard)

mapToken :: TranslateMap -> DevanagariToken -> TS.ShortText
mapToken _ (Unmapped c) = TS.singleton c
mapToken m token =
  let maybeString = Map.lookup token m
   in fromJust maybeString

mapIndependent, mapDependent, mapIast, mapHarvard, mapIso :: DevanagariToken -> TS.ShortText
mapIndependent = mapToken independentMapDevanagari
mapDependent = mapToken dependentMapDevanagari
mapIast = mapToken iastMap
mapHarvard = mapToken harvardMap
mapIso = mapToken isoMap

toDevanagari :: Generator
toDevanagari = translateToDeva TS.empty

translateToDeva :: TS.ShortText -> Generator
translateToDeva acc Empty = acc
translateToDeva acc (x :<| xs) =
  let (firstChars, restTokens) = translateToken x xs
   in translateToDeva (acc `TS.append` firstChars) restTokens
  where
    translateToken :: DevanagariToken -> Seq DevanagariToken -> (TS.ShortText, Seq DevanagariToken)
    translateToken cons@(Cons _) Empty = (mapIndependent cons `TS.append` mapIndependent Virama, Empty)
    translateToken token Empty = (mapIndependent token, Empty)
    translateToken cons@(Cons _) (Vow A :<| ts) = (mapIndependent cons, ts)
    translateToken cons@(Cons _) (vow@(Vow _) :<| ts) = (mapIndependent cons `TS.append` mapDependent vow, ts)
    translateToken cons@(Cons _) ts@(Cons _ :<| _) = (mapIndependent cons `TS.append` mapIndependent Virama, ts)
    translateToken cons@(Cons _) ts@(Unmapped _ :<| _) = (mapIndependent cons `TS.append` mapIndependent Virama, ts)
    translateToken cons@(Cons _) (ZWNJ :<| ts) = (mapIndependent cons `TS.append` mapIndependent Virama `TS.append` mapIndependent ZWNJ, ts)
    translateToken cons@(Cons _) (ZWJ :<| ts) = (mapIndependent cons `TS.append` mapIndependent Virama `TS.append` mapIndependent ZWJ, ts)
    translateToken token tokens@(_ :<| _) = (mapIndependent token, tokens)

toHarvard :: Generator
toHarvard = toTransliteration mapHarvard TS.empty

toIast :: Generator
toIast = toTransliteration mapIast TS.empty

toIso :: Generator
toIso = toTransliteration mapIso TS.empty

toTransliteration :: (DevanagariToken -> TS.ShortText) -> TS.ShortText -> Generator
toTransliteration _f acc Empty = acc
toTransliteration f acc (x :<| xs) = toTransliteration f (acc `TS.append` f x) xs

-- | this function creates a markdown table
--   containing the complete character map in all four encodings.
tokenMapToMd :: TS.ShortText
tokenMapToMd =
  TS.concat $
    tableHeader
      : map
        ( \(hky, dev, ias, iso) ->
            "|" <> hky <> "|" <> dev <> "|" <> ias <> "|" <> iso <> "|\r"
        )
        tokenMap
  where
    tableHeader :: TS.ShortText
    tableHeader = "|Harvard-Kyoto|Devanagari|IAST|ISO15919|\r|----|----|----|----|\r"

-- | this function creates an html table containing the complete character map in all four encodings.
tokenMapToHtml :: TS.ShortText
tokenMapToHtml =
  TS.concat $
    map
      ( \(hky, dev, ias, iso) ->
          "<tr><td>"
            <> hky
            <> "</td><td>"
            <> dev
            <> "</td><td>"
            <> ias
            <> "</td><td>"
            <> iso
            <> "</td></tr>\r"
      )
      tokenMap

-- | returns a list of tuples containing all available characters in all four encodings.
tokenMap :: [(TS.ShortText, TS.ShortText, TS.ShortText, TS.ShortText)]
tokenMap = map (\tok -> (toHarvard tok, toDevanagari tok, toIast tok, toIso tok)) allTokens
  where
    allTokens :: [Seq DevanagariToken]
    allTokens = allVowels ++ allConsonants ++ allDigits ++ allSpecialCharacters

    allVowels :: [Seq DevanagariToken]
    allVowels = map (\v -> fromList [Vow v]) enumerate

    allConsonants :: [Seq DevanagariToken]
    allConsonants = map (\c -> fromList [Cons c, Virama]) enumerate

    allDigits :: [Seq DevanagariToken]
    allDigits = map (\d -> fromList [Dig d]) enumerate

    allSpecialCharacters :: [Seq DevanagariToken]
    allSpecialCharacters = map (\tok -> fromList [tok]) [Anusvara, Anunasika, Visarga, Avagraha, Virama, OM, PurnaViram, DeerghViram, ZWNJ, ZWJ]
