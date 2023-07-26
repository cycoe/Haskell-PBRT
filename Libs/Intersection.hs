module Libs.Intersection where

import Libs.Vector (Vector3)

-- Intersection has two possible constructors. One stands for no intersection,
-- the other stands for intersecting with an object
data Intersection o = Intersection
                      { getCoordinate :: Vector3 Float
                      , getNormal :: Vector3 Float
                      , getObject :: o
                      }
                    deriving Show
