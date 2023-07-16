module Libs.Material.Material where

import Libs.Material.Diffuse (DiffuseMaterial)
import Libs.Vector (Vector3(..), Vector3f, dot, (*.), (.+.), (.-.))
import Libs.Utils (clamp)

cosTheta :: Num t => Vector3 t -> t
cosTheta (Vector3 _ _ c) = c
cos2Theta :: Num t => Vector3 t -> t
cos2Theta (Vector3 _ _ c) = c * c
sin2Theta :: (Num t, Ord t) => Vector3 t -> t
sin2Theta v = max 0 (1 - cos2Theta v)
sinTheta :: (Ord t, Floating t) => Vector3 t -> t
sinTheta v = sqrt $ sin2Theta v
tan2Theta :: (Ord t, Fractional t) => Vector3 t -> t
tan2Theta v = sin2Theta v / cos2Theta v

cosPhi :: Vector3f -> Float
cosPhi w@(Vector3 a b c) =
  let sinThetaw = sinTheta w
  in if sinThetaw == 0 then 1 else clamp (-1) 1 (a / sinThetaw)
sinPhi :: Vector3f -> Float
sinPhi w@(Vector3 a b c) =
  let sinThetaw = sinTheta w
  in if sinThetaw == 0 then 0 else clamp (-1) 1 (b / sinThetaw)

worldToLocal :: Vector3f -> (Vector3f, Vector3f, Vector3f) -> Vector3f
worldToLocal w (a, b, c) = Vector3 _x _y _z where
  c11 = y b * z c - z b * y c
  c12 = z a * y c - y a * z c
  c13 = y a * z b - z a * y b
  c21 = z b * x c - x b * z c
  c22 = x a * z c - x c * z a
  c23 = z a * x b - x a * z b
  c31 = x b * y c - y b * x c
  c32 = y a * x c - x a * y c
  c33 = x a * y b - y a * x b
  detInv = 1 / (x a * c11 + x b * c12 + x c * c13)
  _x = (x w * c11 + y w * c21 + z w * c31) * detInv
  _y = (x w * c12 + y w * c22 + z w * c32) * detInv
  _z = (x w * c13 + y w * c23 + z w * c33) * detInv

localToWorld :: Vector3f -> (Vector3f, Vector3f, Vector3f) -> Vector3f
localToWorld w (a, b, c) = x w *. a .+. y w *. b .+. z w *. c

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

data Material = Diffuse DiffuseMaterial deriving Show
