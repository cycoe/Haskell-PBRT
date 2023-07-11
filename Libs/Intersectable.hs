{-#LANGUAGE InstanceSigs#-}
module Libs.Intersectable where

import Libs.Ray (Ray(..))
import Libs.Vector (Vector(..), dot, normalize)
import Libs.Intersection (Intersection(..))
import Libs.Object.Object (Object(..))
import Libs.Object.Sphere (Sphere(..))
import Libs.Utils (solveQuadratic)

class Intersectable c where
  intersect :: c -> Ray -> Intersection
  intersectP :: c -> Ray -> Bool
  intersectP c ray = case intersect c ray of
    NotIntersect -> False
    Intersection _ _ _ -> True

instance Intersectable Object where
  intersect :: Object -> Ray -> Intersection
  intersect (SphereObject sphere) ray = intersect sphere ray

instance Intersectable Sphere where
  intersect :: Sphere -> Ray -> Intersection
  intersect sphere@(Sphere center radius _) ray =
    let l = getOrigin ray .-. center
        a = getDirection ray `dot` getDirection ray
        b = 2 * getDirection ray `dot` l
        c = dot l l - radius * radius
        _makeIntersect :: Float -> Intersection
        _makeIntersect t = Intersection coords normal (SphereObject sphere) where
          coords = getOrigin ray  .+. t *. getDirection ray
          normal = normalize $ coords .-. center
    in
      case solveQuadratic (a, b, c) of
        Nothing       -> NotIntersect
        Just (t0, t1) ->
          if t1 < 0 then NotIntersect
          else if t0 < 0
          then _makeIntersect t1
          else _makeIntersect t0
