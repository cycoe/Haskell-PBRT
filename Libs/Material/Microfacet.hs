{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE InstanceSigs#-}
module Libs.Material.Microfacet where

import System.Random (RandomGen, uniformR)
import Control.Monad.Trans.State (State, get, put)
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)

import Libs.Vector (Vector(..), Vector3(..), Vector3f, dot, normalize, norm)
import Libs.Material.Base
import Libs.Material.Material
import Libs.Material.RenderMaterial (RenderMaterial(..))
import Libs.Utils (infinity)
import Libs.Tuple (Tuple2)
import Libs.Spectrum (Spectrum(..), SpectrumRGB)
import Libs.Material.Fresnel (Fresnel(..), FrConductor(..))

data TrowbridgeReitzDistribution
  = TrowbridgeReitzDistribution { getDistAlphaX :: Float
                                , getDistAlphaY :: Float
                                , sampleVisibleArea :: Bool
                                } deriving (Show, Generic)

instance NFData TrowbridgeReitzDistribution

class BxDF bxdf where
  sampleWh :: bxdf -> Vector3f -> Tuple2 Float -> Vector3f
  f :: bxdf -> Vector3f -> Vector3f -> Vector3f
  sampleF :: bxdf -> Vector3f -> Vector3f -> (Vector3f, Float, Vector3f)
  pdf :: bxdf -> Vector3f -> Vector3f -> Float

data MicrofacetReflection = MR { getR :: Vector3f
                               , getDistribution :: TrowbridgeReitzDistribution
                               , getFresnel :: FrConductor
                               } deriving (Show, Generic)

instance NFData MicrofacetReflection

instance BxDF MicrofacetReflection where
  sampleWh :: MicrofacetReflection -> Vector3f -> Tuple2 Float -> Vector3f
  sampleWh (MR _ dist _) wo rs = distSampleWh dist wo rs

  f :: MicrofacetReflection -> Vector3f -> Vector3f -> Vector3f
  f (MR r dist conductor) wo wi
    | cosThetaI < 0.001 || cosThetaO < 0.001 = zero
    | norm wh < 0.001 = zero
    | otherwise = _d * _g / (4 * cosThetaI * cosThetaO) *. fr .*. r
    where
      cosThetaI = cosTheta wi
      cosThetaO = cosTheta wo
      wh = wi .+. wo
      whN = normalize wh
      _d = distD dist whN
      _g = distG dist wo wi
      whR = if cosTheta whN < 0 then zero .-. whN else whN
      etaA = 1
      etaB = 1.5
      fr = fresnelFr conductor (wo `dot` whR)

  sampleF :: MicrofacetReflection -> Vector3f -> Vector3f
          -> (Vector3f, Float, Vector3f)
  sampleF bxdf@(MR _ dist _) wo wh
    | z wo == 0 = failed
    | wo `dot` wh < 0 = failed
    | z wo * z wi < 0 = failed
    | otherwise = (wi, _pdf, _fr)
    where
      failed = (Vector3 0 0 0, 0, zero)
      wi = reflectN wo wh
      _pdf = distPdf dist wo wh / (4 * dot wo wh)
      _fr = f bxdf wo wi

  pdf :: MicrofacetReflection -> Vector3f -> Vector3f -> Float
  pdf (MR _ dist _) wo wi
    | z wo * z wi < 0 = 0
    | otherwise = distPdf dist wo wh / (4 * dot wo wh)
    where
      wh = normalize $ wo .+. wi

data BSDF = BSDF { reflection :: MicrofacetReflection
                 } deriving (Show, Generic)

instance NFData BSDF

instance RenderMaterial BSDF where
  sample :: RandomGen g => BSDF -> Vector3f -> State g Vector3f
  sample (BSDF _reflection) wo = do
    g0 <- get
    let (r1, g1) = uniformR (0, 1) g0
        (r2, g2) = uniformR (0, 1) g1
        wh = sampleWh _reflection wo (r1, r2)
        (wi, _, _) = sampleF _reflection wo wh
    put g2
    return wi

  pdf :: BSDF -> Vector3f -> Vector3f -> Float
  pdf (BSDF _reflection) wo wi
    | z wo * z wi > 0 = Libs.Material.Microfacet.pdf _reflection wo wi
    | otherwise = Libs.Material.Microfacet.pdf _reflection wo wi
    where
      wh = if z wo * z wi > 0 then normalize (wo .+. wi) else undefined
      whR = if z wh < 0 then Vector3 0 0 0 .-. wh else wh

  eval :: BSDF -> Vector3f -> Vector3f -> SpectrumRGB
  eval (BSDF _reflection) wi wo = f _reflection wo wi

  getEmission :: BSDF -> SpectrumRGB
  getEmission _ = zero

distD :: TrowbridgeReitzDistribution -> Vector3f -> Float
distD (TrowbridgeReitzDistribution alphaX alphaY _) wh
  | tan2ThetaH == infinity = 0
  | otherwise = 1 / (pi * alphaX * alphaY * cos2ThetaH * cos2ThetaH * e * e)
  where
    cos2ThetaH = cos2Theta wh
    sin2ThetaH = 1 - cos2ThetaH
    tan2ThetaH = sin2ThetaH / cos2ThetaH
    cosPhiH = cosPhi wh
    cos2PhiH = cosPhiH * cosPhiH
    sin2PhiH = 1 - cos2PhiH
    e = 1 + tan2ThetaH * (cos2PhiH / (alphaX * alphaX) +
                          sin2PhiH / (alphaY * alphaY))

distLambda :: TrowbridgeReitzDistribution -> Vector3f -> Float
distLambda (TrowbridgeReitzDistribution alphaX alphaY _) w
  | tan2ThetaW == infinity = 0
  | otherwise = ((-1) + (sqrt $ 1 + alpha2 * tan2ThetaW)) / 2
  where
    cos2ThetaW = cos2Theta w
    sin2ThetaW = 1 - cos2ThetaW
    tan2ThetaW = sin2ThetaW / cos2ThetaW
    cosPhiW = cosPhi w
    cos2PhiW = cosPhiW * cosPhiW
    sin2PhiW = 1 - cos2PhiW
    alpha2 = cos2PhiW * alphaX * alphaX + sin2PhiW * alphaY * alphaY

distG :: TrowbridgeReitzDistribution -> Vector3f -> Vector3f -> Float
distG dist wo wi = 1 / (1 + distLambda dist wo + distLambda dist wi)

distG1 :: TrowbridgeReitzDistribution -> Vector3f -> Float
distG1 dist w = 1 / (1 + distLambda dist w)

trowbridgeReitzSample11 :: Float -> Tuple2 Float -> Tuple2 Float
trowbridgeReitzSample11 cosThetaW (r1, r2)
  | cosThetaW > 0.9999 = 
    let r = sqrt $ r1 / (1 - r1)
        phi = 2 * pi * r2
    in (r * cos phi, r * sin phi)
  | otherwise =
    let sinThetaW = sqrt $ max 0 $ 1 - cosThetaW * cosThetaW
        tanThetaW = sinThetaW / cosThetaW
        a = 1 / tanThetaW
        g1 = 2 / (1 + sqrt (1 + 1 / (a * a)))
        alpha = 2 * r1 / g1 - 1
        tmp = min 1e10 $ 1 / (alpha * alpha - 1)
        beta = tanThetaW
        delta = sqrt $ max 0 $
          beta * beta * tmp * tmp - (alpha * alpha - beta * beta) * tmp
        slopeX1 = beta * tmp - delta
        slopeX2 = beta * tmp + delta
        (s, r) = if r2 > 0.5 then (1, 2 * (r2 - 0.5)) else (-1, 2 * (0.5 - r2))
        z = (r * (r * (r * 0.27385 - 0.73369) + 0.46341)) /
            (r * (r * (r * 0.093073 + 0.309420) - 1.000000) + 0.597999)
        slopeX = if alpha < 0 || slopeX2 > 1 / tanThetaW then slopeX1 else slopeX2
        slopeY = s * z * sqrt (1 + slopeX * slopeX)
    in (slopeX, slopeY)

trowbridgeReitzSample :: Vector3f -> Tuple2 Float -> Tuple2 Float -> Vector3f
trowbridgeReitzSample (Vector3 wx wy wz) (alphaX, alphaY) (r1, r2) =
  normalize $ Vector3 (-slopeX'') (-slopeY'') 1 where
  -- 1. stretch wi
  wiStretched = normalize $ Vector3 (alphaX * wx) (alphaY * wy) wz
  -- 2. simulate P22_{wi}(slopeX, slopeY, 1, 1)
  (slopeX, slopeY) = trowbridgeReitzSample11 (z wiStretched) (r1, r2)
  -- 3. rotate
  cosThetaW = z wiStretched
  r = sqrt $ max 0 $ 1 - cosThetaW * cosThetaW
  slopeX' = x wiStretched / r * slopeX - y wiStretched / r * slopeY
  slopeY' = y wiStretched / r * slopeX + x wiStretched / r * slopeY
  -- 4. unstretch
  slopeX'' = alphaX * slopeX'
  slopeY'' = alphaY * slopeY'

distSampleWh :: TrowbridgeReitzDistribution -> Vector3f -> Tuple2 Float -> Vector3f
distSampleWh (TrowbridgeReitzDistribution alphaX alphaY sva) wo rs
  | sva =
    let _flip = z wo < 0
        wo' = if _flip then (Vector3 0 0 0) .-. wo else wo
        wh = trowbridgeReitzSample wo' (alphaX, alphaY) rs
    in if _flip then (Vector3 0 0 0) .-. wh else wh
  | otherwise = undefined

distPdf :: TrowbridgeReitzDistribution -> Vector3f -> Vector3f -> Float
distPdf dist@(TrowbridgeReitzDistribution _ _ sva) wo wh
  | sva = distD dist wh * distG1 dist wo * abs (wo `dot` wh) / abs (z wo)
  | otherwise = distD dist wh * abs (z wo)
