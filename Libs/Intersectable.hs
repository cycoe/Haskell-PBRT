{-#LANGUAGE InstanceSigs#-}
module Libs.Intersectable where

import Data.Maybe (isJust)
import Libs.Ray (Ray(..))
import Libs.Vector (Vector(..), dot, normalize)
import Libs.Intersection (Intersection(..))
import Libs.Object.Object (Object(..))
import Libs.Object.Sphere (Sphere(..))
import Libs.Utils (solveQuadratic)

class Intersectable c where
  intersect :: c -> Ray -> Maybe Intersection
  intersectP :: c -> Ray -> Bool
  intersectP c ray = isJust $ intersect c ray

instance Intersectable Object where
  intersect :: Object -> Ray -> Maybe Intersection
  intersect (SphereObject sphere) ray = intersect sphere ray

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
