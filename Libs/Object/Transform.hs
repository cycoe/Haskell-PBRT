module Libs.Object.Transform where

import Libs.Vector (Vector3f)

class Transformable t where
  scale :: t -> Float -> t
  move :: t -> Vector3f -> t
