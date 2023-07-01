module Libs.Coordinate where

import Libs.Vector (Vector3, cross)

data Coordinate = Coordinate { getPosition :: Vector3 Float
                             , getFront :: Vector3 Float
                             , getUp :: Vector3 Float
                             } deriving Show

getRight :: Coordinate -> Vector3 Float
getRight (Coordinate _ front up) = cross front up
