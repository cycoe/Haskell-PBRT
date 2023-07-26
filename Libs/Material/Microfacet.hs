module Libs.Material.Microfacet where

import Libs.Vector (Vector3(..), Vector3f)
import Libs.Material.Base
import Libs.Material.Material
import Libs.Utils (infinity)
import Libs.Tuple (Tuple2)

data TrowbridgeReitzDistribution
  = TrowbridgeReitzDistribution { getDistAlphaX :: Float
                                , getDistAlphaY :: Float
                                } deriving Show

d :: TrowbridgeReitzDistribution -> Vector3f -> Float
d (TrowbridgeReitzDistribution alphaX alphaY) wh =
  let cos2Thetah = cos2Theta wh
      sin2Thetah = 1 - cos2Thetah
      tan2Thetah = sin2Thetah / cos2Thetah
  in if tan2Thetah == infinity then 0 else
    let cosPhih = cosPhi wh
        cos2Phih = cosPhih * cosPhih
        sin2Phih = 1 - cos2Phih
        e = 1 + tan2Thetah * (cos2Phih / (alphaX * alphaX) +
                              sin2Phih / (alphaY * alphaY))
    in 1 / (pi * alphaX * alphaY * cos2Thetah * cos2Thetah * e * e)

lambda :: TrowbridgeReitzDistribution -> Vector3f -> Float
lambda (TrowbridgeReitzDistribution alphaX alphaY) w =
  let cos2Thetaw = cos2Theta w
      sin2Thetaw = 1 - cos2Thetaw
      tan2Thetaw = sin2Thetaw / cos2Thetaw
  in if tan2Thetaw == infinity then 0 else
    let cosPhiw = cosPhi w
        cos2Phiw = cosPhiw * cosPhiw
        sin2Phiw = 1 - cos2Phiw
        alpha2 = cos2Phiw * alphaX * alphaX + sin2Phiw * alphaY * alphaY
    in ((-1) + (sqrt $ 1 + alpha2 * tan2Thetaw)) / 2

g :: TrowbridgeReitzDistribution -> Vector3f -> Vector3f -> Float
g dist wo wi = 1 / (1 + lambda dist wo + lambda dist wi)

g1 :: TrowbridgeReitzDistribution -> Vector3f -> Float
g1 dist w = 1 / (1 + lambda dist w)

trowbridgeReitzSample11 :: Float -> Tuple2 Float -> Tuple2 Float
trowbridgeReitzSample11 cosThetaw (r1, r2) =
  if cosThetaw > 0.9999
  then
    let r = sqrt $ r1 / (1 - r1)
        phi = 2 * pi * r2
    in (r * cos phi, r * sin phi)
  else
    let sinThetaw = sqrt $ max 0 $ 1 - cosThetaw * cosThetaw
        tanThetaw = sinThetaw / cosThetaw
        a = 1 / tanThetaw
        g1 = 2 / (1 + sqrt (1 + 1 / (a * a)))
        alpha = 2 * r1 / g1 - 1
        tmp = min 1e10 $ 1 / (alpha * alpha - 1)
        beta = tanThetaw
        delta = sqrt $ max 0 $
          beta * beta * tmp * tmp - (alpha * alpha - beta * beta) * tmp
        slopeX1 = beta * tmp - delta
        slopeX2 = beta * tmp + delta
        (s, r) = if r2 > 0.5 then (1, 2 * (r2 - 0.5)) else (-1, 2 * (0.5 - r2))
        z = (r * (r * (r * 0.27385 - 0.73369) + 0.46341)) /
            (r * (r * (r * 0.093073 + 0.309420) - 1.000000) + 0.597999)
        slopeX = if alpha < 0 || slopeX2 > 1 / tanThetaw then slopeX1 else slopeX2
        slopeY = s * z * sqrt (1 + slopeX * slopeX)
    in (slopeX, slopeY)
