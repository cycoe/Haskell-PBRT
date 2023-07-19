{-#LANGUAGE TemplateHaskell#-}
module Libs.Material.Specular
  (Specular(..)) where

import Control.Lens (makeLenses)
import Libs.Spectrum (SpectrumRGB)

data Specular = Specular { _ks :: SpectrumRGB
                         , _emission :: SpectrumRGB
                         } deriving Show

makeLenses ''Specular
