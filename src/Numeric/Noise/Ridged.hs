-- Copyright (c) 2011, Colin Hill

-- | Implementation of ridged multi-fractal noise.
--
-- Example of use:
--
-- @
--main = putStrLn (\"Noise value at (1, 2, 3): \" ++ show x)
--    where seed        = 1
--          octaves     = 5
--          scale       = 0.005
--          frequency   = 1
--          lacunarity  = 2
--          ridgedNoise = ridged seed octaves scale frequency lacunarity
--          x           = noiseValue ridgedNoise (1, 2, 3)
-- @
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

-- | Constructs a ridged multi-fractal noise function given a seed, number of octaves, scale, 
-- frequency, and lacunarity.
ridged :: Seed -> Int -> Double -> Double -> Double -> Ridged
ridged seed octs scale freq lac = ridgedNoise
    where specWeights = computeSpecWeights octs lac
          ridgedNoise = Ridged seed octs scale freq lac specWeights

instance Noise Ridged where
    noiseValue ridgedNoise xyz = clamp noise (-1) 1
        where Ridged _ octs scale freq _ _ = ridgedNoise
              xyz'  = pmap (* (scale * freq)) xyz
              noise = ridgedNoiseValue ridgedNoise octs 1 xyz' * 1.25 - 1

-- | Computes the noise value for a ridged multi-fractal noise function given the octave number, 
-- the weight, and the point.
ridgedNoiseValue :: Ridged -> Int -> Double -> Point -> Double
ridgedNoiseValue _           0   _      _   = 0
ridgedNoiseValue ridgedNoise oct weight xyz = noise + noise'
    where Ridged seed octs _ _ lac specWeights = ridgedNoise
          oct'    = oct - 1
          xyz'    = pmap (* lac) xyz
          seed'   = (seed + (octs - oct)) .&. 0x7fffffff
          signal  = (offset - abs (coherentNoise seed' xyz)) * weight * weight
          weight' = clamp (signal * gain) 0 1
          noise   = signal * (specWeights ! (octs - oct))
          noise'  = ridgedNoiseValue ridgedNoise oct' weight' xyz'
          gain    = 2
          offset  = 1

-- | Computes the spectral weight for each oct given the number of octs and the lac.
computeSpecWeights :: Int -> Double -> Vector Double
computeSpecWeights octs lac = fromList (computeSpecWeights' octs lac 1)

-- | Helper for 'computeSpecWeights'.
computeSpecWeights' :: Int -> Double -> Double -> [Double]
computeSpecWeights' 0   _   _    = []
computeSpecWeights' oct lac freq = weight : weights
    where freq'   = freq * lac
          oct'    = oct - 1
          weight  = freq ** (-1)
          weights = computeSpecWeights' oct' lac freq'
