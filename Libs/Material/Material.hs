module Libs.Material.Material where

import Libs.Vector (Vector3f, dot, (*.), (.-.))

class RenderMaterial m where
  sample :: m -> Vector3f -> Vector3f -> Vector3f
  pdf :: m -> Vector3f -> Vector3f -> Vector3f -> Float
  eval :: m -> Vector3f -> Vector3f -> Vector3f -> Vector3f
  hasEmission :: m -> Bool
  getEmission :: m -> Vector3f

reflect :: Vector3f -> Vector3f -> Vector3f
reflect i n = 2 * (dot i n) *. n .-. i