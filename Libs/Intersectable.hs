{-#LANGUAGE InstanceSigs#-}
module Libs.Intersectable where

import Libs.Ray (Ray)
import Libs.Intersection (Intersection(..))
import Libs.Object.Object (Object)

class Intersectable c where
  intersect :: c -> Ray -> Intersection
  intersectP :: c -> Ray -> Bool
  intersectP c ray = case intersect c ray of
    NotIntersect -> False
    Intersection _ _ _ -> True

instance Intersectable Object where
  intersect :: Object -> Ray -> Intersection
  intersect = undefined
