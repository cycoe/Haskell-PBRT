module Libs.Object.Object
  (Object(..)) where

import Libs.Object.Sphere (Sphere)
import Libs.Object.Triangle (Triangle)

data Object = SphereObject Sphere
            | TriangleObject Triangle
            deriving Show
