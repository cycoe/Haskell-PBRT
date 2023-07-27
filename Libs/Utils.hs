module Libs.Utils
  ( clamp
  , deg2rad
  , infinity
  , solveQuadratic
  , sumFromHere
  ) where

import Libs.Tuple (Tuple2, Tuple3)

-- Clamp value by range (a, b)
clamp :: Ord t => t -> t -> t -> t
clamp a b v = max a (min b v)

-- Convert degree angle to rad format
deg2rad :: Floating t => t -> t
deg2rad d = d * pi / 180

-- Floating infinity
infinity :: (Floating t, Read t) => t
infinity = read "Infinity"

-- solve quadratic equation
solveQuadratic :: Tuple3 Float -> Maybe (Tuple2 Float)
solveQuadratic (a, b, c) =
  if discr < 0 then Nothing else
    if discr == 0 then Just ((-0.5) * b / a, (-0.5) * b / a)
    else if x0 > x1 then Just (x1, x0) else Just (x0, x1) where
      discr = b * b - 4 * a * c
      q = if b > 0 then (-0.5) * (b + sqrt discr) else (-0.5) * (b - sqrt discr)
      x0 = q / a
      x1 = c / q

-- Calculate sum from head to here
sumFromHere :: Num t => [t] -> [t]
sumFromHere []       = []
sumFromHere [x]      = [x]
sumFromHere (x:y:xs) = x : sumFromHere (x + y : xs)
