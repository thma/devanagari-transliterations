# devanagari-transliterations

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Actions Status](https://github.com/thma/devanagari-transliterations/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/devanagari-transliterations/actions)
[![codecov](https://codecov.io/gh/thma/devanagari-transliterations/graph/badge.svg?token=DBCFLEA8JZ)](https://codecov.io/gh/thma/devanagari-transliterations)

<!--
[![Available on Hackage](https://img.shields.io/hackage/v/devanagari-transliterations.svg?style=flat)](https://github.com/thma/devanagari-transliterations)
-->

This library provides conversions between Devanagari (unicode block 0900-097F) and the transliterations Harvard-Kyoto, IAST and ISO15919.

## Usage

```haskell  
{-# LANGUAGE OverloadedStrings #-}

import Data.Char.Devanagari (tokenize, toDevanagari, toHarvard, toIast, toIso)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  let tokens = tokenize "zubha lAbha"
  putStrLn $ "tokens:        " <> show tokens
  TIO.putStrLn $ "Devanagari:    " <> toDevanagari tokens
  TIO.putStrLn $ "IAST:          " <> toIast tokens
  TIO.putStrLn $ "ISO15919:      " <> toIso tokens
  TIO.putStrLn $ "Harvard Kyoto: " <> toHarvard tokens
```

Output:

```bash
tokens:        fromList [Cons Stop,Vow U,Cons BH,Vow A,Unmapped ' ',Cons L,Vow AA,Cons BH,Vow A]
Devanagari:    शुभ लाभ
IAST:          śubha lābha
ISO15919:      śubha lābha
Harvard Kyoto: zubha lAbha
```

## The complete map of conversions

|Harvard-Kyoto|Devanagari|IAST|ISO15919|
|----|----|----|----|
|a|अ|a|a|
|A|आ|ā|ā|
|i|इ|i|i|
|I|ई|ī|ī|
|u|उ|u|u|
|U|ऊ|ū|ū|
|R|ऋ|ṛ|r̥|
|RR|ॠ|ṝ|r̥̄|
|lR|ऌ|ḷ|l̥|
|lRR|ॡ|ḹ|l̥̄|
|e|ए|e|ē|
|o|ओ|o|ō|
|ai|ऐ|ai|ai|
|au|औ|au|au|
|k|क्|k|k|
|c|च्|c|c|
|T|ट्|ṭ|ṭ|
|t|त्|t|t|
|p|प्|p|p|
|g|ग्|g|g|
|j|ज्|j|j|
|D|ड्|ḍ|ḍ|
|d|द्|d|d|
|b|ब्|b|b|
|G|ङ्|ṅ|ṅ|
|J|ञ्|ñ|ñ|
|N|ण्|ṇ|ṇ|
|n|न्|n|n|
|m|म्|m|m|
|h|ह्|h|h|
|y|य्|y|y|
|r|र्|r|r|
|l|ल्|l|l|
|v|व्|v|v|
|Z|ज़्|z|z|
|f|फ़्|f|f|
|z|श्|ś|ś|
|S|ष्|ṣ|ṣ|
|s|स्|s|s|
|kh|ख्|kh|kh|
|ch|छ्|ch|ch|
|Th|ठ्|ṭh|ṭh|
|th|थ्|th|th|
|ph|फ्|ph|ph|
|gh|घ्|gh|gh|
|jh|झ्|jh|jh|
|Dh|ढ्|ḍh|ḍh|
|dh|ध्|dh|dh|
|bh|भ्|bh|bh|
|ġ|ग़्|ġ|ġ|
|0|०|0|0|
|1|१|1|1|
|2|२|2|2|
|3|३|3|3|
|4|४|4|4|
|5|५|5|5|
|6|६|6|6|
|7|७|7|7|
|8|८|8|8|
|9|९|9|9|
|M|ं|ṃ|ṁ|
|MM|ँ|m̐|m̐|
|H|ः|ḥ|ḥ|
|'|ऽ|'|'|
||्|||
|OM|ॐ|oṃ|ōṁ|
|||।|।|।|
||||॥|॥|॥|
|_|‌|_|_|
|\|‍|\|\|





