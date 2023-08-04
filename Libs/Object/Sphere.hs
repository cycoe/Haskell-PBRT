{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE InstanceSigs#-}
{-#LANGUAGE TemplateHaskell#-}
module Libs.Object.Sphere
  (Sphere(..)) where

import System.Random (uniformR)
import Control.Lens (makeLenses)
import Control.Monad.Trans.State (get, put)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Libs.Ray (Ray(..))
import Libs.Object.Object (Object(..), RenderObject(..), Intersectable(..))
import Libs.Bounds3 (Bounds3(..))
import Libs.Vector (Vector(..), Vector3(..), Vector3f, cross, normalize, dot)
import Libs.Material.Material (Material)
import Libs.Intersection (Intersection(..))
import Libs.Utils (solveQuadratic)

-- Sphere is defined with a center point and a radius
data Sphere = Sphere { _center :: Vector3f
                     , _radius :: Float
                     , _material :: Material
                     , _inside :: Bool
                     } deriving (Show, Generic)

makeLenses ''Sphere

-- | Enable evaluated to NFData
instance NFData Sphere

instance RenderObject Sphere where
  getObjectBounds :: Sphere -> Bounds3
  getObjectBounds (Sphere c r _ _) = Bounds3 (c .- r) (c .+ r)
  getMaterial (Sphere _ _ m _) = m
  sample s@(Sphere c r _ inside) = do
    g0 <- get
    let (r1, g1) = uniformR (0, 1) g0
        (r2, g2) = uniformR (0, 1) g1
        rz = 1 - 2 * r1
        rr = sqrt $ 1 - rz * rz
        phi = 2 * pi * r2
        dir = Vector3 (rr * cos phi) (rr * sin phi) rz
        coord = c .+. r *. dir
        pdf = 1 / getArea s
        o = Object s
        normal = if inside then 0 -. dir else dir
    put g2
    return (Intersection coord normal o, pdf)
  getArea (Sphere c r _ _) = 4 * pi * r * r
  getLocalCS (Sphere c r _ inside) p = (a, b, n) where
    n = normalize $ if inside then c .-. p else p .-. c
    a = normalize $ Vector3 (-(z n)) 0 (x n)
    b = cross n a

instance Intersectable Sphere where
  intersect :: Sphere -> Ray -> Maybe (Intersection Object)
  intersect sphere@(Sphere center radius _ inside) ray =
    let l = getOrigin ray .-. center
        a = getDirection ray `dot` getDirection ray
        b = 2 * getDirection ray `dot` l
        c = dot l l - radius * radius
        _makeIntersect :: Float -> Intersection Object
        _makeIntersect t = Intersection coords normal (Object sphere) where
          coords = getOrigin ray  .+. t *. getDirection ray
          normal = normalize $
            if inside
            then center .-. coords
            else coords .-. center
    in
      case solveQuadratic (a, b, c) of
        Nothing       -> Nothing
        Just (t0, t1) ->
          if t1 < 0.001 then Nothing
          else if t0 < 0.001
          then Just $ _makeIntersect t1
          else Just $ _makeIntersect t0
