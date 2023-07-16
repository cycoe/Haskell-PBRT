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
  intersect :: BVHNode -> Ray -> Maybe Intersection
  intersect (BoxNode b l r) ray@(Ray o d) =
    if intersectP b ray
    then
      let hitL = intersect l ray
          hitR = intersect r ray
      in case (hitL, hitR) of
        (Nothing, Nothing)   -> Nothing
        (Nothing, Just hitR) -> Just hitR
        (Just hitL, Nothing) -> Just hitL
        (Just (Intersection col _ _), Just (Intersection cor _ _)) ->
          if norm (col .-. o) < norm (cor .-. o)
          then hitL
          else hitR
   else Nothing
  intersect (ObjectNode o) ray@(Ray origin _) = do
    i@(Intersection co _ _) <- intersect o ray
    if norm (co .-. origin) < 0.001
    then Nothing
    else return i

instance Intersectable BVHAccelerator where
  intersect :: BVHAccelerator -> Ray -> Maybe Intersection
  intersect (BVHAccelerator _ root) ray = intersect root ray

buildBVHAccelerator :: [Object] -> BVHSplitMethod -> BVHAccelerator
buildBVHAccelerator os sm = BVHAccelerator sm $ _buildBVHNode os sm

getObjects :: BVHAccelerator -> [Object]
getObjects (BVHAccelerator _ root) = _getObjects root

_getObjects :: BVHNode -> [Object]
_getObjects (ObjectNode o) = [o]
_getObjects (BoxNode _ l r) = _getObjects l <> _getObjects r

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
