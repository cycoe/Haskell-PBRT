module Libs.Bounds3 where

import Data.List (zip4)
import Libs.Vector as V
import Libs.Axis (Axis(..))
import Libs.Ray (Ray(..))
import Libs.Utils (infinity)

-- Bounds3 stands for a bounding box in 3D space
-- Bounds3 can described by two vertexes with min (x, y, z) and max (x, y, z)
data Bounds3 = Bounds3 { get_p_min :: V.Vector3f
                       , get_p_max :: V.Vector3f
                       } deriving Show

makeBounds3 :: V.Vector3f -> V.Vector3f -> Bounds3
makeBounds3 p1 p2 = Bounds3 (V.vmin p1 p2) (V.vmax p1 p2)

diagonal :: Bounds3 -> Vector3f
diagonal (Bounds3 p_min p_max) = p_max .-. p_min

max_extent :: Bounds3 -> Axis
max_extent b = let d = diagonal b in
  if x d > y d && x d > z d
  then Axis_x
  else if y d > z d
  then Axis_y
  else Axis_z

centroid :: Bounds3 -> Vector3f
centroid (Bounds3 p_min p_max) = 0.5 *. (p_min .+. p_max)

intersect :: Bounds3 -> Bounds3 -> Bounds3
intersect (Bounds3 pmin1 pmax1) (Bounds3 pmin2 pmax2)=
  Bounds3 (V.vmax pmin1 pmin2) (V.vmin pmax1 pmax2)

union :: Bounds3 -> Bounds3 -> Bounds3
union (Bounds3 pmin1 pmax1) (Bounds3 pmin2 pmax2)=
  Bounds3 (V.vmin pmin1 pmin2) (V.vmax pmax1 pmax2)

unionPoint :: Bounds3 -> Vector3f -> Bounds3
unionPoint (Bounds3 pmin pmax) p =
  Bounds3 (V.vmin pmin p) (V.vmax pmax p)

offset :: Bounds3 -> Vector3f -> Bounds3
offset = undefined

enterExitPanel :: (Float, Float, Float, Float) -> (Float, Float)
enterExitPanel (amin, amax, o, d) =
  let tmin = (amin - o) / d
      tmax = (amax - o) / d
  in if d < 0 then (tmax, tmin) else (tmin, tmax)

intersectP :: Bounds3 -> Ray -> Bool
intersectP (Bounds3 pmin pmax) (Ray origin dir) =
  let times = enterExitPanel <$> zip4
              (toList pmin)
              (toList pmax)
              (toList origin)
              (toList dir)
      (tenter, texit) = foldl
                        (\(m, n) (a, b) -> (max m a, min n b))
                        (-infinity, infinity) times
  in tenter <= texit && texit >= 0
