{-#LANGUAGE InstanceSigs#-}
module Libs.Object.BaseObject
  (RenderObject(..)) where

import Control.Lens ((^.))
import Libs.Bounds3 (Bounds3(..))
import Libs.Object.Object (Object(..))
import Libs.Object.Sphere (Sphere(..))
import Libs.Vector (Vector(..))
import Libs.Material.Material (Material)

-- Renderable object class
class RenderObject ro where
  getObjectBounds :: ro -> Bounds3
  getMaterial :: ro -> Material

instance RenderObject Object where
  getObjectBounds :: Object -> Bounds3
  getObjectBounds (SphereObject o) = getObjectBounds o
  getMaterial :: Object -> Material
  getMaterial (SphereObject o) = getMaterial o

instance RenderObject Sphere where
  getObjectBounds :: Sphere -> Bounds3
  getObjectBounds (Sphere c r _) = Bounds3 (c .- r) (c .+ r)
  getMaterial (Sphere _ _ m) = m
