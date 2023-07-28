{-#LANGUAGE InstanceSigs#-}
module Libs.BVH where

import Data.List (sortBy)
import Libs.Bounds3 (Bounds3(..), intersectP, union, unionPoint, centroid, max_extent)
import Libs.Object.Object (Object, RenderObject(..), Intersectable(intersect))
import Libs.Ray (Ray(..))
import Libs.Intersection (Intersection(..))
import Libs.Vector (norm, (.-.), Vector3(..))
import Libs.Axis (Axis(..))

-- | BVHNode is either a BoxNode pointing to children node or a ObjectNode wrapping
--   an object
data BVHNode o = BoxNode
                 Bounds3      -- ^ Bounds3 contains all objects
                 (BVHNode o)  -- ^ Left node
                 (BVHNode o)  -- ^ Right node
               | ObjectNode
                 o            -- ^ Object wrapped by an ObjectNode
               deriving Show

-- | BVH structure split method, BVHNaiveSplit is implemented
data BVHSplitMethod = BVHNaiveSplit
                    | BVHSAHSplit
                    deriving Show

-- | BVH accelerating structure
data BVHAccelerator o = BVHAccelerator
                        { _splitMethod :: BVHSplitMethod
                        , _rootNode :: BVHNode o
                        }
                      deriving Show

instance Intersectable o => Intersectable (BVHNode o) where
  intersect :: BVHNode o -> Ray -> Maybe (Intersection Object)
  intersect (BoxNode b l r) ray@(Ray o d) =
    if intersectP b ray
    then _nearer (intersect l ray) (intersect r ray) o
    else Nothing
  intersect (ObjectNode o) ray@(Ray origin _) = do
    i@(Intersection co _ _) <- intersect o ray
    if norm (co .-. origin) < 0.0001
    then Nothing
    else return i

instance Intersectable o => Intersectable (BVHAccelerator o) where
  intersect :: BVHAccelerator o -> Ray -> Maybe (Intersection Object)
  intersect (BVHAccelerator _ root) ray = intersect root ray

_nearer :: Maybe (Intersection Object) -> Maybe (Intersection Object)
        -> Vector3 Float -> Maybe (Intersection Object)
_nearer Nothing hitR _ = hitR
_nearer hitL Nothing _ = hitL
_nearer hitL@(Just (Intersection col _ _)) hitR@(Just (Intersection cor _ _)) o
  | norm (col .-. o) < norm (cor .-. o) = hitL
  | otherwise = hitR

buildBVHAccelerator :: RenderObject o => [o] -> BVHSplitMethod -> BVHAccelerator o
buildBVHAccelerator os sm = BVHAccelerator sm $ _buildBVHNode os sm

getObjects :: BVHAccelerator o -> [o]
getObjects (BVHAccelerator _ root) = _getObjects root

_getObjects :: BVHNode o -> [o]
_getObjects (ObjectNode o) = [o]
_getObjects (BoxNode _ l r) = _getObjects l <> _getObjects r

_buildBVHNode :: RenderObject o => [o] -> BVHSplitMethod -> BVHNode o
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

_sortByAxis :: RenderObject o => [o] -> Axis -> [o]
_sortByAxis os Axis_x = sortBy (\a b -> (x . centroid . getObjectBounds $ a) `compare`
                                        (x . centroid . getObjectBounds $ b)) os
_sortByAxis os Axis_y = sortBy (\a b -> (y . centroid . getObjectBounds $ a) `compare`
                                        (y . centroid . getObjectBounds $ b)) os
_sortByAxis os Axis_z = sortBy (\a b -> (z . centroid . getObjectBounds $ a) `compare`
                                        (z . centroid . getObjectBounds $ b)) os

_makeCentroidBounds :: RenderObject o => [o] -> Bounds3
_makeCentroidBounds []     = undefined
_makeCentroidBounds (o:os) = foldl unionPoint (getObjectBounds o) centerPoints where
  centerPoints = centroid . getObjectBounds <$> os

_makeBounds :: RenderObject o => [o] -> Bounds3
_makeBounds [o]    = getObjectBounds o
_makeBounds (o:os) = getObjectBounds o `union` _makeBounds os
_makeBounds _      = undefined
