{-#LANGUAGE InstanceSigs#-}
{-#LANGUAGE TemplateHaskell#-}
module Libs.Material.Specular
  (Specular(..)) where

import Control.Lens (makeLenses)
import Control.Monad.Trans.State (State)
import System.Random (RandomGen)

import Libs.Vector (Vector(..), Vector3(..), Vector3f, dot)
import Libs.Material.RenderMaterial (RenderMaterial(..))
import Libs.Material.Base (reflect)
import Libs.Spectrum (Spectrum(..), SpectrumRGB(..))

data Specular = Specular { _ks :: SpectrumRGB
                         , _emission :: SpectrumRGB
                         } deriving Show

makeLenses ''Specular

instance RenderMaterial Specular where
  sample :: RandomGen g => Specular -> Vector3f -> State g Vector3f
  sample _ wo = pure $ reflect wo

  pdf :: Specular -> Vector3f -> Vector3f -> Float
  pdf _ wo wi = if dot (reflect wo) wi > 0.99 then 1 else 0

  eval :: Specular -> Vector3f -> Vector3f -> Vector3f
  eval (Specular k _) wi wo =
    if dot (reflect wo) wi > 0.99
    then k ./ z wi
    else zero

  getEmission :: Specular -> SpectrumRGB
  getEmission = Libs.Material.Specular._emission
