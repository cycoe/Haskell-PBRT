module Libs.Intersection where

import Libs.Vector (Vector3)

-- | Intersection record the information of intersecting with an object
data Intersection o = Intersection
                      { getCoordinate :: Vector3 Float  -- ^ Intersecting coordinate
                      , getNormal :: Vector3 Float      -- ^ Normal of intersection
                      , getObject :: o                  -- ^ Object that intersection on
                      }
                    deriving Show
