{-#LANGUAGE InstanceSigs#-}
module Libs.Object.BaseObject
  (RenderObject(..)) where

import System.Random (RandomGen, uniformR)
import Control.Monad.Trans.State (State, get, put)
import Libs.Bounds3 (Bounds3(..), makeBounds3, unionPoint)
import Libs.Object.Object (Object(..))
import Libs.Object.Sphere (Sphere(..))
import Libs.Object.Triangle (Triangle(..))
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
  getObjectBounds (TriangleObject o) = getObjectBounds o
  getMaterial :: Object -> Material
  getMaterial (SphereObject o) = getMaterial o
  getMaterial (TriangleObject o) = getMaterial o
  sample :: RandomGen g => Object -> State g (Intersection, Float)
  sample (SphereObject o) = sample o
  sample (TriangleObject o) = sample o
  getArea (SphereObject o) = getArea o
  getArea (TriangleObject o) = getArea o
  getLocalCS (SphereObject o) p = getLocalCS o p
  getLocalCS (TriangleObject o) p = getLocalCS o p

instance RenderObject Sphere where
  getObjectBounds :: Sphere -> Bounds3
  getObjectBounds (Sphere c r _ _) = Bounds3 (c .- r) (c .+ r)
  getMaterial (Sphere _ _ m _) = m
  sample s@(Sphere c r _ inside) = do
    g0 <- get
    let (r1, g1) = uniformR (0, 1) g0
        (r2, g2) = uniformR (0, 1) g1
        theta = 2 * pi * r1
        phi = pi * r2
        dir = Vector3 (sin phi * cos theta) (sin phi * sin theta) (cos phi)
        coord = c .+. r *. dir
        pdf = 1 / getArea s
        o = SphereObject s
        normal = if inside then 0 -. dir else dir
    put g2
    return (Intersection coord normal o, pdf)
  getArea (Sphere c r _ _) = 4 * pi * r * r
  getLocalCS (Sphere c r _ inside) p = (a, b, n) where
    n = normalize $ if inside then c .-. p else p .-. c
    a = normalize $ Vector3 (-(z n)) 0 (x n)
    b = cross n a

instance RenderObject Triangle where
  getObjectBounds (Triangle v0 v1 v2 _ _ _ _ _) = makeBounds3 v0 v1 `unionPoint` v2
  getMaterial = Libs.Object.Triangle._material
  sample t@(Triangle v0 v1 v2 _ _ n a _) = do
    g0 <- get
    let (r1, g1) = uniformR (0, 1) g0
        (r2, g2) = uniformR (0, 1) g1
        rx = sqrt r1
        ry = r2
        coords = (1 - rx) *. v0 .+. (rx * (1 - ry)) *. v1 .+.  rx * ry *. v2
    put g2
    return (Intersection coords n (TriangleObject t), 1 / a)
  getArea = Libs.Object.Triangle._area
  getLocalCS t _ = (a, b, n) where
    n@(Vector3 nx ny nz) = _normal t
    a = if abs ny > 0.999 then Vector3 1 0 0 else normalize $ Vector3 (-nz) 0 nx
    b = n `cross` a
