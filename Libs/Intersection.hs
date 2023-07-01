module Libs.Intersection where

import Libs.Object.Object (Object(..))
import Libs.Vector (Vector3(..))

data Intersection = NotIntersect
                  | Intersection { getCoordinate :: Vector3 Float
                                 , getNormal :: Vector3 Float
                                 , getObject :: Object
                                 }
