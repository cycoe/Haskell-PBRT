{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE InstanceSigs#-}
{-#LANGUAGE TemplateHaskell#-}
module Libs.Material.Glass
  (Glass(..)) where

import Control.Lens (makeLenses)
import Control.Monad.Trans.State (State, get, put)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import System.Random (RandomGen, uniformR)
import Data.Maybe (fromMaybe)

import Libs.Vector (Vector(..), Vector3(..), Vector3f, dot)
import Libs.Material.RenderMaterial (RenderMaterial(..))
import Libs.Material.Base (reflect, refract, frDielectric, cosTheta)
import Libs.Spectrum (Spectrum(..), SpectrumRGB(..))

data Glass = Glass { _ks :: SpectrumRGB
                   , _eta :: Float
                   , _emission :: SpectrumRGB
                   } deriving (Show, Generic)

makeLenses ''Glass

-- | Enable evaluated to NFData
instance NFData Glass

instance RenderMaterial Glass where
  sample :: RandomGen g => Glass -> Vector3f -> State g Vector3f
  sample (Glass _ eta _) wo = do
    g0 <- get
    let (r, g1) = uniformR (0, 1) g0
        cosThetaO = cosTheta wo
        etaI = if cosThetaO > 0 then 1 else eta
        etaT = if cosThetaO > 0 then eta else 1
        fr = frDielectric cosThetaO etaI etaT
        wr = reflect wo
        wt = refract wo (Vector3 0 0 1) (etaI / etaT)
    put g1
    return $ if r < fr then wr else fromMaybe wr wt

  pdf :: Glass -> Vector3f -> Vector3f -> Float
  pdf (Glass _ eta _) wo wi
    | wi `dot` wr > 0.99 = fr
    | wi `dot` wt > 0.99 = 1 - fr
    | otherwise = 0
    where
      cosThetaO = cosTheta wo
      etaI = if cosThetaO > 0 then 1 else eta
      etaT = if cosThetaO > 0 then eta else 1
      fr = frDielectric cosThetaO etaI etaT
      wr = reflect wo
      wt = fromMaybe wr $ refract wo (Vector3 0 0 1) (etaI / etaT)

  eval :: Glass -> Vector3f -> Vector3f -> Vector3f
  eval (Glass k eta _) wi wo
    | wi `dot` wr > 0.99 = fr / cosThetaI *. k
    | wi `dot` wt > 0.99 = (1 - fr) / cosThetaI *. k
    | otherwise = zero
    where
      cosThetaI = abs $ cosTheta wi
      cosThetaO = cosTheta wo
      etaI = if cosThetaO > 0 then 1 else eta
      etaT = if cosThetaO > 0 then eta else 1
      fr = frDielectric cosThetaO etaI etaT
      wr = reflect wo
      wt = fromMaybe wr $ refract wo (Vector3 0 0 1) (etaI / etaT)

  getEmission :: Glass -> SpectrumRGB
  getEmission = Libs.Material.Glass._emission
