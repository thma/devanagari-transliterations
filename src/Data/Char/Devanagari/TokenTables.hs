module Data.Char.Devanagari.TokenTables
  ( harvardKyotoTable,
    iastTable,
    isoTable,
    devanagariIndependentTable,
    devanagariDependentTable,
    inputVirams,
    virams,
    viramsHarvard,
    viramsDeva,
  )
where

{-
  This module contains Token tables for the Devanagari script and its roman transliterations Harvard-Kyoto, IAST and ISO15919.
  These tables are used to generate token ReadMaps for the Tokenizers as well as TranslateMaps for the Generators.
-}

import           Data.Char.Devanagari.DevanagariTokens
import           Data.Text                       (Text, pack)

harvardKyotoTable :: [(Text, DevanagariToken)]
harvardKyotoTable =
  [ ("lRR", Vow LII),
    ("RR", Vow RII),
    ("lR", Vow LI),
    ("OM", OM),
    ("kh", Cons KH),
    ("gh", Cons GH),
    ("ch", Cons CH),
    ("jh", Cons JH),
    ("Th", Cons TdotH),
    ("Dh", Cons DdotH),
    ("th", Cons TH),
    ("dh", Cons DH),
    ("ph", Cons PH),
    ("bh", Cons BH),
    ("k", Cons K),
    ("g", Cons G),
    ("G", Cons Ntop),
    ("c", Cons C),
    ("j", Cons J),
    ("J", Cons Ntild),
    ("T", Cons Tdot),
    ("D", Cons Ddot),
    ("N", Cons Ndot),
    ("t", Cons T),
    ("d", Cons D),
    ("n", Cons N),
    ("p", Cons P),
    ("b", Cons B),
    ("m", Cons M),
    ("y", Cons Y),
    ("r", Cons R),
    ("l", Cons L),
    ("v", Cons V),
    ("z", Cons Stop),
    ("Z", Cons Z),
    ("S", Cons Sdot),
    ("s", Cons S),
    ("h", Cons H),
    ("ai", Vow AI),
    ("au", Vow AU),
    ("ġ", Cons Gtop), -- this is not part of the original HarvardKyoto definition. I added it to allow full compatibility between all transliterations.
    ("f", Cons F),
    ("a", Vow A),
    ("A", Vow AA),
    ("i", Vow I),
    ("I", Vow II),
    ("u", Vow U),
    ("U", Vow UU),
    ("e", Vow E),
    ("o", Vow O),
    ("R", Vow RI),
    ("M", Anusvara),
    ("MM", Anunasika), -- this is not part of the original HarvardKyoto definition. I added it to allow full compatibility between all transliterations.
    ("H", Visarga),
    ("", Virama),
    ("'", Avagraha),
    ("0", Dig ZERO),
    ("1", Dig ONE),
    ("2", Dig TWO),
    ("3", Dig THREE),
    ("4", Dig FOUR),
    ("5", Dig FIVE),
    ("6", Dig SIX),
    ("7", Dig SEVEN),
    ("8", Dig EIGHT),
    ("9", Dig NINE)
  ]

iastTable :: [(Text, DevanagariToken)]
iastTable =
  [ ("kh", Cons KH),
    ("gh", Cons GH),
    ("ch", Cons CH),
    ("jh", Cons JH),
    ("ṭh", Cons TdotH),
    ("ḍh", Cons DdotH),
    ("th", Cons TH),
    ("dh", Cons DH),
    ("ph", Cons PH),
    ("bh", Cons BH),
    ("oṃ", OM),
    ("k", Cons K),
    ("g", Cons G),
    ("ṅ", Cons Ntop),
    ("c", Cons C),
    ("j", Cons J),
    ("ñ", Cons Ntild),
    ("ṭ", Cons Tdot),
    ("ḍ", Cons Ddot),
    ("ṇ", Cons Ndot),
    ("t", Cons T),
    ("d", Cons D),
    ("n", Cons N),
    ("p", Cons P),
    ("b", Cons B),
    ("m", Cons M),
    ("y", Cons Y),
    ("r", Cons R),
    ("l", Cons L),
    ("v", Cons V),
    ("ś", Cons Stop),
    ("ṣ", Cons Sdot),
    ("s", Cons S),
    ("z", Cons Z),
    ("h", Cons H),
    ("ai", Vow AI),
    ("au", Vow AU),
    ("ġ", Cons Gtop),
    ("f", Cons F),
    ("a", Vow A),
    ("ā", Vow AA),
    ("i", Vow I),
    ("ī", Vow II),
    ("u", Vow U),
    ("ū", Vow UU),
    ("e", Vow E),
    ("o", Vow O),
    ("ṛ", Vow RI),
    ("ṝ", Vow RII),
    ("ḷ", Vow LI),
    ("ḹ", Vow LII),
    ("ṃ", Anusvara),
    ("m̐", Anunasika),
    ("ḥ", Visarga),
    ("'", Avagraha),
    ("", Virama),
    ("0", Dig ZERO),
    ("1", Dig ONE),
    ("2", Dig TWO),
    ("3", Dig THREE),
    ("4", Dig FOUR),
    ("5", Dig FIVE),
    ("6", Dig SIX),
    ("7", Dig SEVEN),
    ("8", Dig EIGHT),
    ("9", Dig NINE)
  ]

isoTable :: [(Text, DevanagariToken)]
isoTable =
  [ ("kh", Cons KH),
    ("gh", Cons GH),
    ("ch", Cons CH),
    ("jh", Cons JH),
    ("ṭh", Cons TdotH),
    ("ḍh", Cons DdotH),
    ("th", Cons TH),
    ("dh", Cons DH),
    ("ph", Cons PH),
    ("bh", Cons BH),
    ("ōṁ", OM),
    ("k", Cons K),
    ("g", Cons G),
    ("ṅ", Cons Ntop),
    ("c", Cons C),
    ("j", Cons J),
    ("ñ", Cons Ntild),
    ("ṭ", Cons Tdot),
    ("ḍ", Cons Ddot),
    ("ṇ", Cons Ndot),
    ("t", Cons T),
    ("d", Cons D),
    ("n", Cons N),
    ("p", Cons P),
    ("b", Cons B),
    ("m", Cons M),
    ("y", Cons Y),
    ("r", Cons R),
    ("l", Cons L),
    ("v", Cons V),
    ("ś", Cons Stop),
    ("ṣ", Cons Sdot),
    ("s", Cons S),
    ("z", Cons Z),
    ("h", Cons H),
    ("ai", Vow AI),
    ("au", Vow AU),
    ("ġ", Cons Gtop),
    ("f", Cons F),
    ("a", Vow A),
    ("ā", Vow AA),
    ("i", Vow I),
    ("ī", Vow II),
    ("u", Vow U),
    ("ū", Vow UU),
    ("ē", Vow E),
    ("ō", Vow O),
    ("r̥", Vow RI),
    ("r̥̄", Vow RII),
    ("l̥", Vow LI),
    ("l̥̄", Vow LII),
    ("ṁ", Anusvara),
    ("m̐", Anunasika),
    ("ḥ", Visarga),
    ("'", Avagraha),
    ("", Virama),
    ("0", Dig ZERO),
    ("1", Dig ONE),
    ("2", Dig TWO),
    ("3", Dig THREE),
    ("4", Dig FOUR),
    ("5", Dig FIVE),
    ("6", Dig SIX),
    ("7", Dig SEVEN),
    ("8", Dig EIGHT),
    ("9", Dig NINE)
  ]

devanagariIndependentTable :: [(Text, DevanagariToken)]
devanagariIndependentTable =
  [ ("ख", Cons KH),
    ("घ", Cons GH),
    ("छ", Cons CH),
    ("झ", Cons JH),
    ("ठ", Cons TdotH),
    ("ढ", Cons DdotH),
    ("थ", Cons TH),
    ("ध", Cons DH),
    ("फ", Cons PH),
    ("भ", Cons BH),
    ("ॐ", OM),
    ("क", Cons K),
    ("ग", Cons G),
    ("ग़", Cons Gtop),
    ("ङ", Cons Ntop),
    ("च", Cons C),
    ("ज", Cons J),
    ("ञ", Cons Ntild),
    ("ट", Cons Tdot),
    ("ड", Cons Ddot),
    ("ण", Cons Ndot),
    ("त", Cons T),
    ("द", Cons D),
    ("न", Cons N),
    ("प", Cons P),
    ("ब", Cons B),
    ("म", Cons M),
    ("य", Cons Y),
    ("र", Cons R),
    ("ल", Cons L),
    ("व", Cons V),
    ("श", Cons Stop),
    ("ष", Cons Sdot),
    ("स", Cons S),
    ("ह", Cons H),
    ("ग", Cons G),
    ("ज़", Cons Z),
    ("फ़", Cons F),
    ("अ", Vow A),
    ("आ", Vow AA),
    ("इ", Vow I),
    ("ई", Vow II),
    ("उ", Vow U),
    ("ऊ", Vow UU),
    ("ए", Vow E),
    ("ओ", Vow O),
    ("ऋ", Vow RI),
    ("ॠ", Vow RII),
    ("ऌ", Vow LI),
    ("ॡ", Vow LII),
    ("ऐ", Vow AI),
    ("औ", Vow AU),
    ("्", Virama),
    ("ं", Anusvara),
    ("ँ", Anunasika),
    ("ः", Visarga),
    ("ऽ", Avagraha),
    ("०", Dig ZERO),
    ("१", Dig ONE),
    ("२", Dig TWO),
    ("३", Dig THREE),
    ("४", Dig FOUR),
    ("५", Dig FIVE),
    ("६", Dig SIX),
    ("७", Dig SEVEN),
    ("८", Dig EIGHT),
    ("९", Dig NINE)
  ]

devanagariDependentTable :: [(Text, DevanagariToken)]
devanagariDependentTable =
  [ ("ं", Anusvara),
    ("ँ", Anunasika),
    ("ः", Visarga),
    ("ऽ", Avagraha),
    ("ा", Vow AA),
    ("ि", Vow I),
    ("ी", Vow II),
    ("ु", Vow U),
    ("ू", Vow UU),
    ("ृ", Vow RI),
    ("ॄ", Vow RII),
    ("ॢ", Vow LI),
    ("ॣ", Vow LII),
    ("े", Vow E),
    ("ै", Vow AI),
    ("ो", Vow O),
    ("ौ", Vow AU)
  ]

-- The handling for virams is not symmetric for input and output.
-- This is meant as an optimization to ease manual input by using |, ||,
-- _ representing the ZWNJ (zero width non joiner)
-- \ representing the ZWJ (zero width joiner)
inputVirams :: [(Text, DevanagariToken)]
inputVirams =
  [ ("|", PurnaViram),
    ("||", DeerghViram),
    ("।", PurnaViram),
    ("॥", DeerghViram),
    ("_", ZWNJ),
    ("\\", ZWJ),
    (pack ['\8204'], ZWNJ),
    (pack ['\8205'], ZWJ)
  ]

virams :: [(Text, DevanagariToken)]
virams =
  [ ("।", PurnaViram),
    ("॥", DeerghViram),
    ("_", ZWNJ),
    ("\\", ZWJ)
  ]

viramsHarvard :: [(Text, DevanagariToken)]
viramsHarvard =
  [ ("|", PurnaViram),
    ("||", DeerghViram),
    ("_", ZWNJ),
    ("\\", ZWJ)
  ]

-- Devanagari needs special unicode characters for ZWNJ and ZWJ
viramsDeva :: [(Text, DevanagariToken)]
viramsDeva =
  [ ("।", PurnaViram),
    ("॥", DeerghViram),
    (pack ['\8204'], ZWNJ),
    (pack ['\8205'], ZWJ)
  ]
