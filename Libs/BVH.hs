{-#LANGUAGE InstanceSigs#-}
module Libs.BVH where

import Libs.Bounds3 (Bounds3(..), intersectP)
import Libs.Object.Object (Object(..))
import Libs.Ray (Ray(..), Intersectable(intersect))
import Libs.Intersection (Intersection(..))
import Libs.Vector (norm, (.-.))

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

instance Intersectable BVHNode where
  intersect :: BVHNode -> Ray -> Intersection
  intersect (BoxNode b l r) ray@(Ray o d) =
    if intersectP b ray
    then
      let hitL = intersect l ray
          hitR = intersect r ray
      in case (hitL, hitR) of (NotIntersect, hitR) -> hitR
                              (hitL, NotIntersect) -> hitL
                              (Intersection col _ _, Intersection cor _ _) ->
                                if norm (col .-. o) < norm (cor .-. o)
                                then hitL
                                else hitR
   else NotIntersect
  intersect (ObjectNode o) ray = intersect o ray

instance Intersectable BVHAccelerator where
  intersect :: BVHAccelerator -> Ray -> Intersection
  intersect (BVHAccelerator _ root) ray = intersect root ray