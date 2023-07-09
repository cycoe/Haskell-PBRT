{-#LANGUAGE InstanceSigs#-}
module Libs.Object.BaseObject
  (RenderObject(..)) where

import Libs.Bounds3 (Bounds3(..))
import Libs.Vector (Vector(..))
import Libs.Object.Object (Object(..))
import Libs.Object.Sphere (Sphere(..))

-- Renderable object class
class RenderObject ro where
  getObjectBounds :: ro -> Bounds3

instance RenderObject Object where
  getObjectBounds :: Object -> Bounds3
  getObjectBounds (SphereObject o) = getObjectBounds o

instance RenderObject Sphere where
  getObjectBounds :: Sphere -> Bounds3
  getObjectBounds (Sphere c r) = Bounds3 (c .- r) (c .+ r)
