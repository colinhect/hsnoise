-- Copyright (c) 2011, Colin Hill

-- | Implementation of Perlin noise.
--
-- Simple example of using Perlin noise:
--
-- @
-- n = noiseValue perlinNoise (x, y, z)
--     where seed = 1
--           octaves = 10
--           scale = 0.1
--           pers = 0.5
--           perlinNoise = perlin seed octaves scale pers
-- @
module Numeric.Noise.Perlin (
    Perlin,
    perlin,
    noiseValue
) where

import Numeric.Noise

-- | A Perlin noise function.
data Perlin = Perlin Seed Int Double Double

-- | Implementation of 'Noise' for 'Perlin'.
instance Noise Perlin where
    noiseValue nf xyz = clamp noise (-1) 1
        where Perlin _ octaves _ _ = nf
              noise                = perlinNoiseValue nf octaves 0 1.0 1.0 xyz

-- | Constructs a Perlin noise function given a 'Seed', number of octaves, scale, and persistance.
perlin :: Seed -> Int -> Double -> Double -> Perlin
perlin seed octaves scale pers = Perlin seed octaves scale pers

-- | Evaluates a given number of octaves of perlin noise holding the total noise value, frequency
-- and amplitude in the arguments.
perlinNoiseValue :: Perlin -> Int -> Double -> Double -> Double -> Point -> Double
perlinNoiseValue _  0 noise _ _ _   = noise
perlinNoiseValue nf o noise f a xyz = noise + perlinNoiseValue nf o' noise' f' a' xyz
    where Perlin seed _ scale pers = nf
          (x, y, z)                = xyz
          (x', y', z')             = (x * scale * f, y * scale * f, z * scale * f)
          o'                       = o - 1 -- Next octave.
          noise'                   = coherentNoise seed (x', y', z') * a -- Next noise value.
          f'                       = f * 2 -- Next frequency.
          a'                       = a * pers -- Next amplitude.
