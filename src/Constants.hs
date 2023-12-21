module Constants
  ( nil,
    false,
    true,
    fxShift,
    fxTag,
    fxMask,
    charShift,
    charTag,
  )
where

import Data.Int (Int32)

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

charShift :: Int
charShift = 8

charTag :: Int32
charTag = 0x0F
