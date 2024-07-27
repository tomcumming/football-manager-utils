module FM.Maths where

import Data.Foldable1 (Foldable1)

weightedMean :: (Foldable1 f, Functor f) => f (Double, Double) -> Double
weightedMean xs = sum (uncurry (*) <$> xs) / sum (snd <$> xs)

weightedStdev :: (Foldable1 f, Functor f) => f (Double, Double) -> (Double, Double)
weightedStdev xs = (m, sqrt (sum (uncurry f <$> xs) / sum (snd <$> xs)))
  where
    m = weightedMean xs
    f x w = (x - m) ** 2 * w

asZScore :: (Double, Double) -> Double -> Double
asZScore (m, s) x = (x - m) / s
