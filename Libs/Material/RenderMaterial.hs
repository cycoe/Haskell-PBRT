{-#LANGUAGE InstanceSigs#-}
module Libs.Material.RenderMaterial
  (RenderMaterial(..)) where

import System.Random (RandomGen, uniformR)
import Control.Monad.Trans.State (State, get, put)
import Libs.Material.Material (Material(..), reflect)
import Libs.Material.Diffuse (DiffuseMaterial(..))
import Libs.Material.Specular (Specular(..))
import Libs.Vector (Vector3f, Vector3(..), Vector(..), norm, dot)
import Libs.Spectrum (SpectrumRGB, zero)

-- Material type class
class RenderMaterial m where
  -- Sample wi by wo from material surface, random generator is wrapped by state
  -- Vector3f -> wo
  -- State g Vector3f -> wi sampled from wo wrapped by state
  sample :: RandomGen g => m -> Vector3f -> State g Vector3f
  -- pdf of wi sampled by wo
  -- Vector3f -> wo
  -- Vector3f -> wi
  pdf :: m -> Vector3f -> Vector3f -> Float
  -- bxdf of wi and wo
  -- Vector3f -> wi
  -- Vector3f -> wo
  -- Spectrum -> bxdf described by spectrum
  eval :: m -> Vector3f -> Vector3f -> SpectrumRGB
  getEmission :: m -> SpectrumRGB
  hasEmission :: m -> Bool
  hasEmission m = norm (getEmission m) > 0.001

instance RenderMaterial Material where
  sample :: RandomGen g => Material -> Vector3f -> State g Vector3f
  sample (Diffuse m) wo = sample m wo
  sample (SpecularMaterial m) wo = sample m wo
  pdf :: Material -> Vector3f -> Vector3f -> Float
  pdf (Diffuse m) wo wi = pdf m wo wi
  pdf (SpecularMaterial m) wo wi = pdf m wo wi
  eval :: Material -> Vector3f -> Vector3f -> SpectrumRGB
  eval (Diffuse m) wi wo = eval m wi wo
  eval (SpecularMaterial m) wi wo = eval m wi wo
  getEmission (Diffuse m) = getEmission m
  getEmission (SpecularMaterial m) = getEmission m

instance RenderMaterial DiffuseMaterial where
  sample :: RandomGen g => DiffuseMaterial -> Vector3f -> State g Vector3f
  sample _ _ = do
    gen <- get
    let (r1, gen') = uniformR (0, 1) gen
        (r2, gen'') = uniformR (0, 1) gen'
        z = abs $ 1 - 2 * r1
        r = sqrt $ 1 - z * z
        phi = 2 * pi * r2
    put gen''
    return $ Vector3 (r * cos phi) (r * sin phi) z

  pdf :: DiffuseMaterial -> Vector3f -> Vector3f -> Float
  pdf _ _ wi = if z wi > 0 then 0.5 / pi else 0

  eval :: DiffuseMaterial -> Vector3f -> Vector3f -> Vector3f
  eval (DiffuseMaterial k _) _ wo = if z wo > 0 then k ./ pi else Vector3 0 0 0

  getEmission :: DiffuseMaterial -> SpectrumRGB
  getEmission = Libs.Material.Diffuse._emission

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
