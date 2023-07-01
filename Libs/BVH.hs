module Libs.BVH where

import Libs.Bounds3 (Bounds3(..))
import Libs.Object.Object (Object(..))
import Libs.Ray (Ray)
import Libs.Intersection (Intersection)

data BVHNode = BoxNode { getBounds :: Bounds3
                       , getLeft :: BVHNode
                       , getRight :: BVHNode
                       }
             | ObjectNode Object

data BVHSplitMethod = BVHNaiveSplit
                    | BVHSAHSplit
                    deriving Show

data BVHAccelerator = BVHAccelerator { getSplitMethod :: BVHSplitMethod
                                     , getRootNode :: BVHNode
                                     }

intersect :: BVHNode -> Ray -> Intersection
intersect = undefined
