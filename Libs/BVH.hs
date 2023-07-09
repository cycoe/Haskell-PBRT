{-#LANGUAGE InstanceSigs#-}
module Libs.BVH where

import Data.List (sortBy)
import Libs.Bounds3 (Bounds3(..), intersectP, union, unionPoint, centroid, max_extent)
import Libs.Object.Object (Object(..))
import Libs.Object.BaseObject (RenderObject(getObjectBounds))
import Libs.Ray (Ray(..))
import Libs.Intersection (Intersection(..))
import Libs.Intersectable (Intersectable(intersect))
import Libs.Vector (norm, (.-.), Vector3(..))
import Libs.Axis (Axis(..))

data BVHNode = BoxNode { getBounds :: Bounds3
                       , getLeft   :: BVHNode
                       , getRight  :: BVHNode
                       }
             | ObjectNode { getObject :: Object
                          }
             deriving Show

data BVHSplitMethod = BVHNaiveSplit
                    | BVHSAHSplit
                    deriving Show

data BVHAccelerator = BVHAccelerator { getSplitMethod :: BVHSplitMethod
                                     , getRootNode :: BVHNode
                                     } deriving Show

instance Intersectable BVHNode where
  intersect :: BVHNode -> Ray -> Intersection
  intersect (BoxNode b l r) ray@(Ray o d) =
    if intersectP b ray
    then
      let hitL = intersect l ray
          hitR = intersect r ray
      in case (hitL, hitR) of
        (NotIntersect, hitR) -> hitR
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

buildBVHAccelerator :: [Object] -> BVHSplitMethod -> BVHAccelerator
buildBVHAccelerator os sm = BVHAccelerator sm $ _buildBVHNode os sm

_buildBVHNode :: [Object] -> BVHSplitMethod -> BVHNode
_buildBVHNode [] _       = undefined
_buildBVHNode [o] _      = ObjectNode o
_buildBVHNode [o1, o2] _ = BoxNode bounds left right where
  bounds = _makeBounds [o1, o2]
  left = ObjectNode o1
  right = ObjectNode o2
_buildBVHNode os sm      = BoxNode bounds left right where
  bounds = _makeBounds os
  left = _buildBVHNode leftShapes sm
  right = _buildBVHNode rightShapes sm
  (leftShapes, rightShapes) = splitAt middle sorted
  middle = length os `div` 2
  sorted = _sortByAxis os . max_extent . _makeCentroidBounds $ os

_sortByAxis :: [Object] -> Axis -> [Object]
_sortByAxis os Axis_x = sortBy (\a b -> (x . centroid . getObjectBounds $ a) `compare`
                                        (x . centroid . getObjectBounds $ b)) os
_sortByAxis os Axis_y = sortBy (\a b -> (y . centroid . getObjectBounds $ a) `compare`
                                        (y . centroid . getObjectBounds $ b)) os
_sortByAxis os Axis_z = sortBy (\a b -> (z . centroid . getObjectBounds $ a) `compare`
                                        (z . centroid . getObjectBounds $ b)) os

_makeCentroidBounds :: [Object] -> Bounds3
_makeCentroidBounds []     = undefined
_makeCentroidBounds (o:os) = foldl unionPoint (getObjectBounds o) centerPoints where
  centerPoints = centroid . getObjectBounds <$> os

_makeBounds :: [Object] -> Bounds3
_makeBounds [o]    = getObjectBounds o
_makeBounds (o:os) = getObjectBounds o `union` _makeBounds os
_makeBounds _      = undefined
