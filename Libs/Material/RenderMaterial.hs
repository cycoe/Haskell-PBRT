{-#LANGUAGE InstanceSigs#-}
module Libs.Material.RenderMaterial
  (RenderMaterial(..)) where

import System.Random (RandomGen)
import Control.Monad.Trans.State (State)
import Libs.Vector (Vector3f, norm)
import Libs.Spectrum (SpectrumRGB)

-- | Render-able material class type
class RenderMaterial m where
  -- | Sample wi by wo from material surface, random generator is wrapped by state
  sample :: RandomGen g
         => m                 -- ^ Material to sample
         -> Vector3f          -- ^ wo
         -> State g Vector3f  -- ^ wi sampled from wo wrapped by state

  -- | Pdf of wi sampled by wo
  pdf :: m
      -> Vector3f  -- ^ wo
      -> Vector3f  -- ^ wi
      -> Float     -- ^ pdf of wi sampled by wo

  -- | bxdf of wi and wo
  eval :: m
       -> Vector3f     -- ^ wi
       -> Vector3f     -- ^ wo
       -> SpectrumRGB  -- ^ bxdf described by spectrum
  getEmission :: m -> SpectrumRGB
  hasEmission :: m -> Bool
  hasEmission m = norm (getEmission m) > 0.001
