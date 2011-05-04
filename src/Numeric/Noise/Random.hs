-- Copyright (c) 2011, Colin Hill

-- | A simple implementation of a pure linear congruential psuedo-random number generator.
module Numeric.Noise.Random (
    randomInt,
    randomInts,
    shuffle
) where

import Numeric.Noise

import Data.Bits

-- | Returns a random 'Int' and the next seed given a seed.
randomInt :: Seed -> (Int, Seed)
randomInt seed = randomInt' seed 0 16

-- | Helper for 'randomInt'.
randomInt' :: Int -> Seed -> Int -> (Int, Seed)
randomInt' r seed 0 = (r, seed)
randomInt' r seed n = randomInt' r' seed' (n - 1)
    where seed' = 3039177861 * seed + 1
          r'    = shiftL r 2 + shiftR seed' 30

-- | Returns a random sequence of 'Int's given a seed and the number of 'Int's to generate.
randomInts :: Seed -> Int -> [Int]
randomInts _    0 = []
randomInts seed n = r : randomInts seed' (n - 1)
    where (r, seed') = randomInt seed

-- | Returns a shuffled list containing the same elements as the given list given a seed.
shuffle :: [a] -> Seed -> [a]
shuffle xs seed = fst (shuffle' xs [] seed)

-- | Helper function for 'shuffle'.
shuffle' :: [a] -> [a] -> Seed -> ([a], Seed)
shuffle' [] acc seed = (acc, seed)
shuffle' xs acc seed = shuffle' (h ++ ys) (y:acc) seed'
    where (seed', r) = randomInt seed
          (h, y:ys)  = splitAt (r `mod` length xs) xs
