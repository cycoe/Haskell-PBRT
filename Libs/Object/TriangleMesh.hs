{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE InstanceSigs#-}
module Libs.Object.TriangleMesh where

import System.Random (RandomGen)
import Control.Monad.Trans.State (State)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Libs.Vector (Vector3f)
import Libs.Object.Object (Object(..), RenderObject(..), Intersectable(..))
import Libs.Object.Triangle (Triangle(..))
import Libs.Ray (Ray)
import Libs.Bounds3 (Bounds3)
import Libs.BVH (BVHAccelerator(..), BVHSplitMethod(..), buildBVHAccelerator, getBounds, sample)
import Libs.Material.Material (Material)
import Libs.Intersection (Intersection(..))

data TriangleMesh = TM { _bvh :: BVHAccelerator Triangle
                       , _area :: Float
                       , _material :: Material
                       } deriving (Show, Generic)

makeTriangleMesh :: [Triangle] -> Material -> TriangleMesh
makeTriangleMesh ts m = TM bvh area m where
  -- Update material for each triangle
  ts' = (\t -> t {Libs.Object.Triangle._material = m}) <$> ts
  bvh = buildBVHAccelerator ts' BVHNaiveSplit
  area = sum $ Libs.Object.Triangle._area <$> ts

-- | Enable evaluated to NFData
instance NFData TriangleMesh

instance RenderObject TriangleMesh where
  getObjectBounds :: TriangleMesh -> Bounds3
  getObjectBounds = getBounds . _bvh

  getMaterial :: TriangleMesh -> Material
  getMaterial = Libs.Object.TriangleMesh._material

  sample :: RandomGen g => TriangleMesh -> State g (Intersection Object, Float)
  sample (TM bvh _ _) = Libs.BVH.sample bvh

  getArea :: TriangleMesh -> Float
  getArea = Libs.Object.TriangleMesh._area

  getLocalCS :: TriangleMesh -> Vector3f -> (Vector3f, Vector3f, Vector3f)
  getLocalCS = undefined

instance Intersectable TriangleMesh where
  intersect :: TriangleMesh -> Ray -> Maybe (Intersection Object)
  intersect (TM bvh _ m) ray = intersect bvh ray
