module Libs.Object.Sphere
  (Sphere(..)) where

import Libs.Vector (Vector3f)

-- Sphere is defined with a center point and a radius
data Sphere = Sphere { center :: Vector3f
                     , radius :: Float
                     } deriving Show
