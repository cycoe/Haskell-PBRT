module Libs.Object.Triangle
  ( Triangle(..)
  , makeTriangle
  ) where

import Data.Maybe (fromMaybe)

import Libs.Vector (Vector(..), Vector3f, cross, norm, normalize)
import Libs.Material.Material (Material)

data Triangle = Triangle { _v0 :: Vector3f
                         , _v1 :: Vector3f
                         , _v2 :: Vector3f
                         , _e1 :: Vector3f
                         -- ^ Edge: v0 -> v1
                         , _e2 :: Vector3f
                         -- ^ Edge: v0 -> v2
                         , _normal :: Vector3f
                         -- ^ Normal of triangle. It should be provided by object file,
                         -- otherwise it's calculated by (v1 - v0) `cross` (v2 - v0)
                         , _area :: Float
                         , _material :: Material
                         } deriving Show

makeTriangle :: Vector3f -- ^ v0
             -> Vector3f -- ^ v1
             -> Vector3f -- ^ v2
             -> Maybe Vector3f -- ^ normal of triangle, it can be provided or not
             -> Material -- ^ material bind to triangle
             -> Triangle
makeTriangle v0 v1 v2 mn m = Triangle v0 v1 v2 e1 e2 n a m where
  e1 = v1 .-. v0
  e2 = v2 .-. v0
  a = 0.5 * norm (cross e1 e2)
  n = fromMaybe (normalize $ cross e1 e2) mn
