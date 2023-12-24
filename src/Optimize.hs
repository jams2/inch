{-# LANGUAGE Strict #-}

module Optimize (optimizeImmediates) where

import Arch
import Asm

pairwiseWith :: (Line -> Line -> Maybe Program) -> Program -> Program
pairwiseWith _ [] = []
pairwiseWith f xs = loop xs
  where
    loop :: Program -> Program
    loop [] = []
    loop [x] = [x]
    loop (x : y : rest) = case f x y of
      Nothing -> x : loop (y : rest)
      Just p -> p ++ loop rest

optimizeImmediates :: Program -> Program
optimizeImmediates = pairwiseWith f
  where
    f
      (Instruction (Mov (Immediate v) (Register RAX)))
      ( Instruction
          ( Mov
              (Register RAX)
              (Offset i r)
            )
        ) = Just [mov (Immediate v) (i % r)]
    f (Instruction (Mov p q)) (Instruction (Mov q' p'))
      | (p == p') && (q == q') = Just []
      | otherwise = Nothing
    f _ _ = Nothing
