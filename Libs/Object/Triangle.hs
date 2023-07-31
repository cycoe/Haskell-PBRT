{-#LANGUAGE InstanceSigs#-}
{-#LANGUAGE TemplateHaskell#-}
module Libs.Object.Triangle
  ( Triangle(..)
  , makeTriangle
  ) where

import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State (get, put)
import System.Random (uniformR)

import Libs.Ray (Ray(..), transport)
import Libs.Object.Object (Object(..), RenderObject(..), Intersectable(..))
import Libs.Bounds3 (makeBounds3, unionPoint)
import Libs.Vector (Vector(..), Vector3(..), Vector3f, cross, norm, normalize, dot)
import Libs.Intersection (Intersection(..))
import Libs.Material.Material (Material)
import Libs.Object.Transform (Transformable(..))

data Triangle = Triangle { _v0 :: Vector3f
                         , _v1 :: Vector3f
                         , _v2 :: Vector3f
                         , _e1 :: Vector3f
                         -- ^ Edge: v0 -> v1
                         , _e2 :: Vector3f
                         -- ^ Edge: v0 -> v2
                         , _normal :: Vector3f
                         -- ^ Normal of triangle. It should be provided by object file,
                         -- otherwise it's calculated by (v1 - v0) `cross` (v2 - v0)
                         , _area :: Float
                         , _material :: Material
                         } deriving Show

makeTriangle :: Vector3f -- ^ v0
             -> Vector3f -- ^ v1
             -> Vector3f -- ^ v2
             -> Maybe Vector3f -- ^ normal of triangle, it can be provided or not
             -> Material -- ^ material bind to triangle
             -> Triangle
makeTriangle v0 v1 v2 mn m = Triangle v0 v1 v2 e1 e2 n a m where
  e1 = v1 .-. v0
  e2 = v2 .-. v0
  a = 0.5 * norm (cross e1 e2)
  n = fromMaybe (normalize $ cross e1 e2) mn

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
    return (Intersection coords n (Object t), 1 / a)
  getArea = Libs.Object.Triangle._area
  getLocalCS t _ = (a, b, n) where
    n@(Vector3 nx ny nz) = _normal t
    a = if abs ny > 0.999 then Vector3 1 0 0 else normalize $ Vector3 (-nz) 0 nx
    b = n `cross` a

instance Intersectable Triangle where
  intersect :: Triangle -> Ray -> Maybe (Intersection Object)
  intersect t@(Triangle v0 _ _ e1 e2 n _ _) ray@(Ray o d)
    | abs det < 0.001    = Nothing
    | u < 0 || u > 1     = Nothing
    | v < 0 || u + v > 1 = Nothing
    | ttmp < 0.001       = Nothing
    | otherwise          = Just $ Intersection coords n (Object t)
    where
      pvec = d `cross` e2
      det = e1 `dot` pvec
      detInv = 1 / det
      tvec = o .-. v0
      u = detInv * (tvec `dot` pvec)
      qvec = tvec `cross` e1
      v = detInv * (d `dot` qvec)
      ttmp = detInv * (e2 `dot` qvec)
      coords = transport ray ttmp

instance Transformable Triangle where
  scale :: Triangle -> Float -> Triangle
  scale (Triangle v0 v1 v2 e1 e2 n a m) s = Triangle v0' v1' v2' e1' e2' n a' m where
    v0' = s *. v0
    v1' = s *. v1
    v2' = s *. v2
    e1' = s *. e1
    e2' = s *. e2
    a'  = s * s * a

  move :: Triangle -> Vector3f -> Triangle
  move (Triangle v0 v1 v2 e1 e2 n a m) v = Triangle v0' v1' v2' e1 e2 n a m where
    v0' = v .+. v0
    v1' = v .+. v1
    v2' = v .+. v2
