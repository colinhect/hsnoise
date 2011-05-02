module Noise.Perlin (
    Perlin(..),
    noise
) where

import Noise

-- | Perlin noise function.
data Perlin =
    -- | Constructs a perlin noise function given a 'Seed', number of octaves, scale, and 
    -- persistance.
    Perlin Seed Int Double Double

-- | Implementation of 'Noise' for 'Perlin'.
instance Noise Perlin where
    noise n p = max (-1.0) (min 1.0 v)
        where Perlin seed octs scale pers = n
              v = perlinNoise n octs 0 1.0 1.0 p

-- | Evaluates a given number of octaves of perlin noise holding the total and the amplitude
-- and frequency in the arguments.
perlinNoise :: Perlin -> Int -> Double -> Double -> Double -> Point -> Double
perlinNoise n 0 t freq amp p = t
perlinNoise n o t freq amp p = t + perlinNoise n (o - 1) t' (freq * 2.0) (amp * pers) p
    where Perlin seed octs scale pers = n
          (x, y, z) = p
          x'        = x * scale * freq
          y'        = y * scale * freq
          z'        = z * scale * freq
          t'        = coherentNoise seed (x', y', z') * amp
