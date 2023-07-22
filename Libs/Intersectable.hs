{-#LANGUAGE InstanceSigs#-}
module Libs.Intersectable where

import Data.Maybe (isJust)
import Libs.Ray (Ray(..), transport)
import Libs.Vector (Vector(..), dot, normalize, cross)
import Libs.Intersection (Intersection(..))
import Libs.Object.Object (Object(..))
import Libs.Object.Sphere (Sphere(..))
import Libs.Object.Triangle (Triangle(..))
import Libs.Utils (solveQuadratic)

class Intersectable c where
  intersect :: c -> Ray -> Maybe Intersection
  intersectP :: c -> Ray -> Bool
  intersectP c ray = isJust $ intersect c ray

instance Intersectable Object where
  intersect :: Object -> Ray -> Maybe Intersection
  intersect (SphereObject sphere) ray = intersect sphere ray
  intersect (TriangleObject t) ray = intersect t ray

instance Intersectable Sphere where
  intersect :: Sphere -> Ray -> Maybe Intersection
  intersect sphere@(Sphere center radius _ inside) ray =
    let l = getOrigin ray .-. center
        a = getDirection ray `dot` getDirection ray
        b = 2 * getDirection ray `dot` l
        c = dot l l - radius * radius
        _makeIntersect :: Float -> Intersection
        _makeIntersect t = Intersection coords normal (SphereObject sphere) where
          coords = getOrigin ray  .+. t *. getDirection ray
          normal = normalize $
            if inside
            then center .-. coords
            else coords .-. center
    in
      case solveQuadratic (a, b, c) of
        Nothing       -> Nothing
        Just (t0, t1) ->
          if t1 < 0.001 then Nothing
          else if t0 < 0.001
          then Just $ _makeIntersect t1
          else Just $ _makeIntersect t0

instance Intersectable Triangle where
  intersect :: Triangle -> Ray -> Maybe Intersection
  intersect t@(Triangle v0 _ _ e1 e2 n _ _) ray@(Ray o d)
    | abs det < 0.001    = Nothing
    | u < 0 || u > 1     = Nothing
    | v < 0 || u + v > 1 = Nothing
    | ttmp < 0.001       = Nothing
    | otherwise          = Just $ Intersection coords n (TriangleObject t)
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
