{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE InstanceSigs#-}
module Libs.BVH where

import Data.List (sortBy)
import System.Random (RandomGen, uniformR)
import Control.Monad.Trans.State (State, get, put)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

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
                 Float        -- ^ Area sum of all objects
               | ObjectNode
                 o            -- ^ Object wrapped by an ObjectNode
               deriving (Show, Generic)

-- | BVH structure split method, BVHNaiveSplit is implemented
data BVHSplitMethod = BVHNaiveSplit
                    | BVHSAHSplit
                    deriving (Show, Generic)

-- | BVH accelerating structure
data BVHAccelerator o = BVHAccelerator
                        { _splitMethod :: BVHSplitMethod
                        , _rootNode :: BVHNode o
                        }
                      deriving (Show, Generic)

-- | Enable evaluated to NFData
instance NFData BVHSplitMethod

-- | Enable evaluated to NFData
instance NFData o => NFData (BVHNode o)

-- | Enable evaluated to NFData
instance NFData o => NFData (BVHAccelerator o)

instance Intersectable o => Intersectable (BVHNode o) where
  intersect :: BVHNode o -> Ray -> Maybe (Intersection Object)
  intersect (BoxNode b l r _) ray@(Ray o d) =
    if intersectP b ray
    then _nearer (intersect l ray) (intersect r ray) o
    else Nothing
  intersect (ObjectNode o) ray@(Ray origin _) = do
    i@(Intersection co _ _) <- intersect o ray
    if norm (co .-. origin) < 0.001
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

getBounds :: RenderObject o => BVHAccelerator o -> Bounds3
getBounds (BVHAccelerator _ root) = _getRootBounds root

getArea :: RenderObject o => BVHNode o -> Float
getArea (BoxNode _ _ _ a) = a
getArea (ObjectNode o) = Libs.Object.Object.getArea o

sample :: (RandomGen g, RenderObject o) => BVHAccelerator o
       -> State g (Intersection Object, Float)
sample (BVHAccelerator _ root) = do
  g0 <- get
  let (r1, g1) = uniformR (0, 1) g0
      p = sqrt r1 * Libs.BVH.getArea root
  put g1
  (i, pdf) <- _sampleBVHNode root p
  return (i, pdf / Libs.BVH.getArea root)

_sampleBVHNode :: (RandomGen g, RenderObject o) => BVHNode o -> Float
               -> State g (Intersection Object, Float)
_sampleBVHNode (BoxNode _ l r _) p =
  if p < Libs.BVH.getArea l
  then _sampleBVHNode l p
  else _sampleBVHNode r (p - Libs.BVH.getArea l)
_sampleBVHNode (ObjectNode o) _ = do
  (i, p) <- Libs.Object.Object.sample o
  return (i, Libs.Object.Object.getArea o * p)

_getRootBounds :: RenderObject o => BVHNode o -> Bounds3
_getRootBounds (ObjectNode o) = getObjectBounds o
_getRootBounds (BoxNode b _ _ _) = b

_getObjects :: BVHNode o -> [o]
_getObjects (ObjectNode o) = [o]
_getObjects (BoxNode _ l r _) = _getObjects l <> _getObjects r

_buildBVHNode :: RenderObject o => [o] -> BVHSplitMethod -> BVHNode o
_buildBVHNode [] _       = undefined
_buildBVHNode [o] _      = ObjectNode o
_buildBVHNode [o1, o2] _ = BoxNode bounds left right area where
  bounds = _makeBounds [o1, o2]
  left = ObjectNode o1
  right = ObjectNode o2
  area = Libs.Object.Object.getArea o1 + Libs.Object.Object.getArea o2
_buildBVHNode os sm      = BoxNode bounds left right area where
  bounds = _makeBounds os
  left = _buildBVHNode leftShapes sm
  right = _buildBVHNode rightShapes sm
  (leftShapes, rightShapes) = splitAt middle sorted
  middle = length os `div` 2
  sorted = _sortByAxis os . max_extent . _makeCentroidBounds $ os
  area = Libs.BVH.getArea left + Libs.BVH.getArea right

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
