{-#LANGUAGE TemplateHaskell#-}
module Libs.Object.Sphere
  (Sphere(..)) where

import Control.Lens (makeLenses)
import Libs.Vector (Vector3f)
import Libs.Material.Material (Material)

-- Sphere is defined with a center point and a radius
data Sphere = Sphere { _center :: Vector3f
                     , _radius :: Float
                     , _material :: Material
                     } deriving Show

makeLenses ''Sphere
