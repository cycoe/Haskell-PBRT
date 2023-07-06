{-#LANGUAGE InstanceSigs#-}
module Libs.Material.Diffuse where

import System.Random (RandomGen, uniformR)
import Control.Monad.Trans.State (State, get, put)
import Libs.Vector (Vector3f, Vector3(..), (./), norm)
import Libs.Material.Material (RenderMaterial(..))
import Libs.Spectrum (Spectrum)

data DiffuseMaterial = DiffuseMaterial { emission :: Vector3f
                                       , kd :: Vector3f
                                       } deriving Show

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
  eval (DiffuseMaterial _ k) _ wo = if z wo > 0 then k ./ pi else Vector3 0 0 0

  hasEmission :: DiffuseMaterial -> Bool
  hasEmission (DiffuseMaterial e _) = norm e > 0.00001
  getEmission :: DiffuseMaterial -> Vector3f
  getEmission = emission
