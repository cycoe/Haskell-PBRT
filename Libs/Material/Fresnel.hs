{-#LANGUAGE DeriveGeneric#-}
module Libs.Material.Fresnel where

import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)

import Libs.Vector (Vector(..), Vector3(..), Vector3f, vsqrt)
import Libs.Spectrum (SpectrumRGB)
import Libs.Utils (clamp)

-- | Fresnel rule
class Fresnel f where
  fresnelFr :: f
            -> Float       -- ^ cos theta of incident ray
            -> SpectrumRGB -- ^ fresnel cofficient of material interface

data FrDielectric = FrDielectric
  Float -- ^ eta of incident ray
  Float -- ^ eta of transmission ray
  deriving (Show, Generic)

instance NFData FrDielectric

instance Fresnel FrDielectric where
  fresnelFr (FrDielectric etaI etaT) cosThetaI
    | sinThetaT < 1 = Vector3 fr fr fr
    | otherwise = Vector3 1 1 1
    where
      cosThetaI' = clamp (-1) 1 cosThetaI
      entering = cosThetaI' > 0
      (etaI', etaT') = if entering then (etaI, etaT) else (etaT, etaI)
      cosThetaI'' = abs cosThetaI'
      sinThetaI = sqrt $ max 0 (1 - cosThetaI'' * cosThetaI'')
      sinThetaT = etaI' / etaT' * sinThetaI
      cosThetaT = sqrt $ max 0 (1 - sinThetaT * sinThetaT)
      rparl = ((etaT' * cosThetaI'') - (etaI' * cosThetaT)) /
              ((etaT' * cosThetaI'') + (etaI' * cosThetaT))
      rperp = ((etaI' * cosThetaI'') - (etaT' * cosThetaT)) /
              ((etaI' * cosThetaI'') + (etaT' * cosThetaT))
      fr = (rparl * rparl + rperp * rperp) / 2

data FrConductor = FrConductor
  SpectrumRGB -- ^ eta of incident ray
  SpectrumRGB -- ^ eta of transmission ray
  SpectrumRGB -- ^ some cofficient k
  deriving (Show, Generic)

instance NFData FrConductor

instance Fresnel FrConductor where
  fresnelFr (FrConductor etaI etaT k) cosThetaI = 0.5 *. (rp .+. rs) where
    cosThetaI' = clamp (-1) 1 cosThetaI
    cos2ThetaI = cosThetaI' * cosThetaI'
    sin2ThetaI = 1 - cos2ThetaI
    eta = etaT ./. etaI
    eta2 = eta .*. eta
    etaK = k ./. etaI
    etaK2 = etaK .*. etaK
    t0 = eta2 .-. etaK2 .- sin2ThetaI
    c2 = vsqrt $ t0 .*. t0 .+. 4 *. eta2 .*. etaK2
    t1 = c2 .+ cos2ThetaI
    a = vsqrt $ 0.5 *. (c2 .+. t0)
    t2 = 2 * cosThetaI' *. a
    rs = (t1 .-. t2) ./. (t1 .+. t2)
    t3 = cos2ThetaI *. c2 .+ sin2ThetaI * sin2ThetaI
    t4 = t2 .* sin2ThetaI
    rp = rs .*. (t3 .-. t4) ./. (t3 .+. t4)
