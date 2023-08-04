{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE InstanceSigs#-}
{-#LANGUAGE TemplateHaskell#-}
module Libs.Material.Diffuse
  (Diffuse(..)) where

import Control.Lens (makeLenses)
import Control.Monad.Trans.State (State, get, put)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import System.Random (RandomGen, uniformR)

import Libs.Material.RenderMaterial (RenderMaterial(..))
import Libs.Vector (Vector(..), Vector3(..), Vector3f, norm)
import Libs.Spectrum (SpectrumRGB)

data Diffuse = Diffuse { _kd :: SpectrumRGB
                       , _emission :: SpectrumRGB
                       } deriving (Show, Generic)

makeLenses ''Diffuse

-- | Enable evaluated to NFData
instance NFData Diffuse

instance RenderMaterial Diffuse where
  sample :: RandomGen g => Diffuse -> Vector3f -> State g Vector3f
  sample _ _ = do
    gen <- get
    let (r1, gen') = uniformR (0, 1) gen
        (r2, gen'') = uniformR (0, 1) gen'
        z = abs $ 1 - 2 * r1
        r = sqrt $ 1 - z * z
        phi = 2 * pi * r2
    put gen''
    return $ Vector3 (r * cos phi) (r * sin phi) z

  pdf :: Diffuse -> Vector3f -> Vector3f -> Float
  pdf _ _ wi = if z wi > 0 then 0.5 / pi else 0

  eval :: Diffuse -> Vector3f -> Vector3f -> Vector3f
  eval (Diffuse k _) _ wo = if z wo > 0 then k ./ pi else Vector3 0 0 0

  getEmission :: Diffuse -> SpectrumRGB
  getEmission = Libs.Material.Diffuse._emission
