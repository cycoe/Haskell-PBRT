{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE ExistentialQuantification#-}
module Libs.Material.Material where

import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)

import Libs.Material.RenderMaterial (RenderMaterial(..))

-- | Material is a heterogeneous type wrapper of class RenderMaterial
data Material = forall a. ( Show a
                          , RenderMaterial a
                          , Generic a
                          , NFData a
                          ) => Material a

instance Generic Material

-- | Enable evaluated to NFData
instance NFData Material where
  rnf (Material m) = rnf m

instance Show Material where
  show (Material m) = show m

instance RenderMaterial Material where
  sample (Material m) wo = sample m wo
  pdf (Material m) wo wi = pdf m wo wi
  eval (Material m) wi wo = eval m wi wo
  getEmission (Material m) = getEmission m
  hasEmission (Material m) = hasEmission m
