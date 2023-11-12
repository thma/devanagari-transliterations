module Data.Char.Devanagari
  ( DevanagariToken (..),
    Vowel (..),
    Consonant (..),
    Digit (..),
    Tokenizer,
    tokenize,
    selectTokenizerByContent,
    fromDevanagari,
    fromIso,
    fromHarvard,
    fromIast,
    toDevanagari,
    toIso,
    toHarvard,
    toIast,
    tokenMap,
    tokenMapToMd,
    tokenMapToHtml,
  )
where

import           Data.Char.Devanagari.DevanagariTokens
import           Data.Char.Devanagari.Generators
import           Data.Char.Devanagari.Tokenizer
