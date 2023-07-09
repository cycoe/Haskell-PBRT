module Libs.Object.Object
  (Object(..)) where

import Libs.Object.Sphere (Sphere)

data Object = SphereObject Sphere deriving Show
