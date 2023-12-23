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
    Text,
    pack,
    unpack
  )
where

import           Data.Char.Devanagari.DevanagariTokens
import           Data.Char.Devanagari.Generators
import           Data.Char.Devanagari.Tokenizer
import           Data.Text                     (Text, pack, unpack)
