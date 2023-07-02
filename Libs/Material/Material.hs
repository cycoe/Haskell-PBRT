module Libs.Material.Material where

import Libs.Vector (Vector3(Vector3), Vector3f, dot, (*.), (.+.), (.-.))

class RenderMaterial m where
  sample :: m -> Vector3f -> Vector3f -> Vector3f
  pdf :: m -> Vector3f -> Vector3f -> Vector3f -> Float
  eval :: m -> Vector3f -> Vector3f -> Vector3f -> Vector3f
  hasEmission :: m -> Bool
  getEmission :: m -> Vector3f

cosTheta :: Num t => Vector3 t -> t
cosTheta (Vector3 _ _ c) = c
cos2Theta :: Num t => Vector3 t -> t
cos2Theta (Vector3 _ _ c) = c * c
sin2Theta :: (Num t, Ord t) => Vector3 t -> t
sin2Theta v = max 0 (1 - cos2Theta v)
sinTheta :: (Ord t, Floating t) => Vector3 t -> t
sinTheta v = sqrt $ sin2Theta v

reflect :: Vector3f -> Vector3f -> Vector3f
reflect wi n = 2 * (dot wi n) *. n .-. wi

-- Refract of interface
-- param wi -> in light
-- param n -> normal vector of interface
-- param eta -> index ratio of interface
-- return -> Just wo if wo exists
--           Nothing if fully reflection happened
refract :: Vector3f -> Vector3f -> Float -> Maybe Vector3f
refract wi n eta =
  let cosThetaI = dot wi n
      sin2ThetaI = max 0 $ 1 - cosThetaI * cosThetaI
      sin2ThetaT = eta * eta * sin2ThetaI
  in if sin2ThetaT >= 1
     then Nothing
     else let cosThetaT = sqrt $ 1 - sin2ThetaT
          in Just $ (-eta) *. wi .+. (eta * cosThetaI - cosThetaT) *. n