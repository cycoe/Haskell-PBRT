{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE InstanceSigs#-}
module Libs.Material.Diffuse
  (DiffuseMaterial(..)) where

import Control.Lens (makeLenses)
import Libs.Spectrum (SpectrumRGB)

data DiffuseMaterial = DiffuseMaterial { _kd :: SpectrumRGB
                                       , _emission :: SpectrumRGB
                                       } deriving Show

makeLenses ''DiffuseMaterial
