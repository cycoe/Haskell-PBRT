{-#LANGUAGE InstanceSigs#-}
module Libs.Ray where

import Libs.Vector (Vector3, (.+.), (*.))

data Ray = Ray { getOrigin :: Vector3 Float
               , getDirection :: Vector3 Float
               } deriving Show

transport :: Ray -> Float -> Vector3 Float
transport (Ray o d) t = o .+. (t *. d)
