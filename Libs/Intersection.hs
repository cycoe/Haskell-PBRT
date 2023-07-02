module Libs.Intersection where

import Libs.Object.Object (Object(..))
import Libs.Vector (Vector3(..))

-- Intersection has two possible constructors. One stands for no intersection,
-- the other stands for intersecting with an object
data Intersection = NotIntersect
                  | Intersection { getCoordinate :: Vector3 Float
                                 , getNormal :: Vector3 Float
                                 , getObject :: Object
                                 }
