module Data.Char.Devanagari.DevanagariTokens (
    DevanagariToken(..)
  , Vowel(..)
  , Consonant(..)
  , Digit(..)
)

where

{-
  This module contains a normalized representation of the Devanagari script.

  A Tokenizer for Devanagari or one of its roman transliterations will parse a UTF8 Text representation and
  produce a Sequence of DevanagariToken instances as output.

  A Generator for Devanagari or one of its roman transliterations will take a sequence of DevanagariToken instances
  as input an will generate a UTF8 Text representation as output.
-}

data DevanagariToken
  = Vow Vowel | Cons Consonant | Dig Digit | Anusvara | Anunasika | Visarga | Avagraha | Virama | OM 
  | PurnaViram | DeerghViram | ZWNJ | ZWJ | Unmapped Char
  deriving (Eq, Ord, Show)

data Vowel
  = A | AA | I | II | U | UU | RI | RII | LI | LII | E | O | AI | AU
  deriving (Eq, Ord, Bounded, Enum, Show)

data Consonant
  = K | C | Tdot | T | P | G | J | Ddot | D | B | Ntop | Ntild
  | Ndot | N | M | H | Y | R | L | V | Z | F | Stop | Sdot | S
  | KH | CH | TdotH | TH | PH | GH | JH | DdotH | DH | BH| Gtop
  deriving (Eq, Ord, Bounded, Enum, Show)

data Digit = ZERO | ONE | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE
  deriving (Eq, Ord, Bounded, Enum, Show)
