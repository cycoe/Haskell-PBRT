module Libs.Ray where

import Libs.Vector (Vector3, (.+.), (*.))
import Libs.Intersection (Intersection)

data Ray = Ray { getOrigin :: Vector3 Float
               , getDirection :: Vector3 Float
               } deriving Show

class Collisionable c where
  intersect :: c -> Ray -> Intersection

transport :: Ray -> Float -> Vector3 Float
transport (Ray o d) t = o .+. (t *. d)
