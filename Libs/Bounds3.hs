module Libs.Bounds3 where

import Libs.Vector as V
import Libs.Axis (Axis(..))

data Bounds3 = Bounds3 { get_p_min :: V.Vector3f
                       , get_p_max :: V.Vector3f
                       } deriving Show

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

offset :: Bounds3 -> Vector3f -> Bounds3
offset = undefined
