module Libs.Material.Base where

import Libs.Vector (Vector(..), Vector3(..), Vector3f, dot)
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

reflect :: Vector3f -> Vector3f
reflect (Vector3 a b c) = Vector3 (-a) (-b) c

-- | Refraction of interface
refract :: Vector3f       -- ^ direction of incident ray
        -> Vector3f       -- ^ normal of interface
        -> Float          -- ^ eta ratio of (etaI / etaT)
        -> Maybe Vector3f -- ^ Just wo if wo exists
                          --   Nothing if total internal reflection happened
refract wi n eta
  | sin2ThetaT < 1 = Just $ (-eta) *. wi .+. (eta * cosThetaI' - cosThetaT) *. n'
  -- Handle total internal reflection transmission
  | otherwise = Nothing
  where
    cosThetaI = dot wi n
    -- reverse normal along wi direction
    n' = if cosThetaI < 0 then (Vector3 0 0 0) .-. n else n
    cosThetaI' = abs cosThetaI
    sin2ThetaI = max 0 $ 1 - cosThetaI' * cosThetaI'
    sin2ThetaT = eta * eta * sin2ThetaI
    cosThetaT = sqrt $ 1 - sin2ThetaT

-- | Fresnel of dielectric materials
frDielectric :: Float -- ^ cos theta of incident ray
             -> Float -- ^ eta of incident ray
             -> Float -- ^ eta of transmission ray
             -> Float -- ^ fresnel cofficient of dielectric material interface
frDielectric cosThetaI etaI etaT
  | sinThetaT < 1 = (rparl * rparl + rperp * rperp) / 2
  | otherwise = 1
  where
    cosThetaI' = clamp (-1) 1 cosThetaI
    entering = cosThetaI' > 0
    (etaI', etaT') = if entering then (etaI, etaT) else (etaT, etaI)
    cosThetaI'' = abs cosThetaI'
    sinThetaI = sqrt $ max 0 (1 - cosThetaI'' * cosThetaI'')
    sinThetaT = etaI / etaT * sinThetaI
    cosThetaT = sqrt $ max 0 (1 - sinThetaT * sinThetaT)
    rparl = ((etaT' * cosThetaI'') - (etaI' * cosThetaT)) /
            ((etaT' * cosThetaI'') + (etaI' * cosThetaT))
    rperp = ((etaI' * cosThetaI'') - (etaT' * cosThetaT)) /
            ((etaI' * cosThetaI'') + (etaT' * cosThetaT))
