-- Copyright (c) 2011, Colin Hill

module Noise.Perlin (
    Perlin(..),
    noiseValue
) where

import Noise

-- | Perlin noise function.
data Perlin =
    -- | Constructs a perlin noise function given a 'Seed', number of octaves, scale, and 
    -- persistance.
    Perlin Seed Int Double Double

-- | Implementation of 'Noise' for 'Perlin'.
instance Noise Perlin where
    -- | Returns a perlin noise value given a 'Perlin' function and a point.
    noiseValue nf xyz = max (-1.0) (min 1.0 noise)
        where Perlin _ octs _ _ = nf
              noise             = perlinNoise nf octs 0 1.0 1.0 xyz

-- | Evaluates a given number of octaves of perlin noise holding the total, amplitude
-- and frequency in the arguments.
perlinNoise :: Perlin -> Int -> Double -> Double -> Double -> Point -> Double
perlinNoise nf 0 t freq amp xyz = t
perlinNoise nf o t freq amp xyz = t + perlinNoise nf (o - 1) t' (freq * 2.0) (amp * persist) xyz
    where Perlin seed _ scale persist = nf
          (x, y, z)                   = xyz
          (x', y', z')                = (x * scale * freq, y * scale * freq, z * scale * freq)
          t'                          = coherentNoise seed (x', y', z') * amp
