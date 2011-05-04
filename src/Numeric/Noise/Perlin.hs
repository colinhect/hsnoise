-- Copyright (c) 2011, Colin Hill

-- | Implementation of Perlin noise.
--
-- Example of use:
--
-- @
--main = putStrLn (\"Noise value at (1, 2, 3): \" ++ show x)
--    where seed        = 1
--          octaves     = 5
--          scale       = 0.05
--          persistance = 0.5
--          perlinNoise = perlin seed octaves scale persistance
--          x           = noiseValue perlinNoise (1, 2, 3)
-- @
module Numeric.Noise.Perlin (
    Perlin,
    perlin,
    noiseValue
) where

import Numeric.Noise

-- | A Perlin noise function.
data Perlin = Perlin Seed Int Double Double

instance Noise Perlin where
    noiseValue perlinNoise xyz = clamp noise (-1) 1
        where Perlin _ octs _ _ = perlinNoise
              noise             = perlinNoiseValue perlinNoise octs 1 1 xyz

-- | Constructs a Perlin noise function given a seed, number of octaves, scale, and persistance.
perlin :: Seed -> Int -> Double -> Double -> Perlin
perlin = Perlin

-- | Evaluates a given number of octaves of perlin noise holding the frequency and amplitude in 
-- the arguments.
perlinNoiseValue :: Perlin -> Int -> Double -> Double -> Point -> Double
perlinNoiseValue _           0   _    _   _   = 0
perlinNoiseValue perlinNoise oct freq amp xyz = noise + noise'
    where Perlin seed _ scale pers = perlinNoise
          oct'   = oct - 1
          freq'  = freq * 2
          amp'   = amp * pers
          xyz'   = pmap (* (scale * freq)) xyz
          noise  = coherentNoise seed xyz' * amp
          noise' = perlinNoiseValue perlinNoise oct' freq' amp' xyz
