module Constants
  ( nil,
    false,
    true,
    fxShift,
    fxTag,
    fxMask,
    charMask,
    charShift,
    charTag,
    boolMask,
    wordSize,
  )
where

import Data.Int (Int32)

wordSize :: Int
wordSize = 8 -- bytes

nil :: Int32
nil = 0x3F

false :: Int32
false = 0x2F

true :: Int32
true = 0x6F

fxShift :: Int
fxShift = 2

fxTag :: Int32
fxTag = 0

fxMask :: Int
fxMask = 3

charMask :: Int
charMask = 0x3f

charShift :: Int
charShift = 8

charTag :: Int32
charTag = 0x0F

boolMask :: Int32
boolMask = 0xBF -- boolMask & (T or F) give F. Doesn't clash with null
