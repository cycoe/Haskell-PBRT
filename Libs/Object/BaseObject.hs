{-#LANGUAGE InstanceSigs#-}
module Libs.Object.BaseObject
  (RenderObject(..)) where

import System.Random (RandomGen, uniformR)
import Control.Monad.Trans.State (State, get, put)
import Libs.Bounds3 (Bounds3(..))
import Libs.Object.Object (Object(..))
import Libs.Object.Sphere (Sphere(..))
import Libs.Vector (Vector(..), Vector3(..), Vector3f, normalize, cross)
import Libs.Material.Material (Material)
import Libs.Intersection (Intersection(..))

-- Renderable object class
class RenderObject ro where
  getObjectBounds :: ro -> Bounds3
  getMaterial :: ro -> Material
  -- sample on object returns a tuple of intersect point and pdf
  sample :: RandomGen g => ro -> State g (Intersection, Float)
  getArea :: ro -> Float
  getLocalCS :: ro -> Vector3f -> (Vector3f, Vector3f, Vector3f)

instance RenderObject Object where
  getObjectBounds :: Object -> Bounds3
  getObjectBounds (SphereObject o) = getObjectBounds o
  getMaterial :: Object -> Material
  getMaterial (SphereObject o) = getMaterial o
  sample :: RandomGen g => Object -> State g (Intersection, Float)
  sample (SphereObject o) = sample o
  getArea (SphereObject o) = getArea o
  getLocalCS (SphereObject o) p = getLocalCS o p

instance RenderObject Sphere where
  getObjectBounds :: Sphere -> Bounds3
  getObjectBounds (Sphere c r _) = Bounds3 (c .- r) (c .+ r)
  getMaterial (Sphere _ _ m) = m
  sample s@(Sphere c r _) = do
    g0 <- get
    let (r1, g1) = uniformR (0, 1) g0
        (r2, g2) = uniformR (0, 1) g1
        theta = 2 * pi * r1
        phi = pi * r2
        dir = Vector3 (cos phi) (sin phi * cos theta) (sin phi * sin theta)
        coord = c .+. r *. dir
        pdf = 1 / getArea s
        o = SphereObject s
    put g2
    return (Intersection coord dir o, pdf)
  getArea (Sphere c r _) = 4 * pi * r * r
  getLocalCS (Sphere c r _) p = (a, b, n) where
    n = normalize $ p .-. c
    a = normalize $ Vector3 (-(z n)) 0 (x n)
    b = cross n a
