-- Copyright (c) 2011, Colin Hill

-- | Implementation of ridged multi-fractal noise.
module Numeric.Noise.Ridged (
    Ridged,
    ridged,
    noiseValue
) where

import Numeric.Noise

import Data.Bits ((.&.))
import Data.Vector.Unboxed (Vector, fromList, (!))

-- | A ridged multi-fractal noise function.
data Ridged = Ridged Seed Int Double Double Double (Vector Double)

-- | Constructs a ridged multi-fractal noise function given a seed, octaves, scale, frequency, and
-- lacunarity.
ridged :: Seed -> Int -> Double -> Double -> Double -> Ridged
ridged seed octs scale freq lac = Ridged seed octs scale freq lac (spectralWeights octs lac)

-- | Instance of 'Noise' for 'Ridged'.
instance Noise Ridged where
    noiseValue nf (x, y, z) = clamp noise (-1) 1
        where Ridged _ octs scale freq _ _ = nf
              xyz   = (x * scale * freq, y * scale * freq, z * scale * freq)
              noise = ridgedNoiseValue nf octs 0 1 xyz

ridgedNoiseValue :: Ridged -> Int -> Double -> Double -> Point -> Double
ridgedNoiseValue _  0 noise _ _   = noise
ridgedNoiseValue nf o noise w xyz = noise + ridgedNoiseValue nf o' noise' w' xyz'
    where Ridged seed octs _ _ lac ws = nf
          o'           = o - 1
          (x, y, z)    = xyz
          xyz'         = (x * lac, y * lac,  z * lac)
          seed'        = (seed + (octs - o)) .&. 0x7fffffff
          signal       = 1.0 - (abs (coherentNoise seed' (x, y, z))) * w * w
          w'           = clamp (signal * 2) 0 1
          noise'       = signal * (ws ! (octs - o))

-- | Computes the spectral weight for each octave given the number of octaves and the lacunarity.
spectralWeights :: Int -> Double -> Vector Double
spectralWeights o l = fromList (reverse (spectralWeights' o l 1))

-- | Helper for 'spectralWeights'.
spectralWeights' :: Int -> Double -> Double -> [Double]
spectralWeights' 0 _ _ = [] 
spectralWeights' o l f = f ** (-1) : spectralWeights' o' l f' 
    where f' = f * l
          o' = o - 1
