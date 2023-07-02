{-#LANGUAGE InstanceSigs#-}
module Libs.Ray where

import Libs.Vector (Vector3, (.+.), (*.))
import Libs.Intersection (Intersection)
import Libs.Object.Object (Object)

data Ray = Ray { getOrigin :: Vector3 Float
               , getDirection :: Vector3 Float
               } deriving Show

class Intersectable c where
  intersect :: c -> Ray -> Intersection

instance Intersectable Object where
  intersect :: Object -> Ray -> Intersection
  intersect = undefined

transport :: Ray -> Float -> Vector3 Float
transport (Ray o d) t = o .+. (t *. d)