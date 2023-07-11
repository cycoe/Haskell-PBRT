{-#LANGUAGE TemplateHaskell #-}
module Libs.Scene
 ( Scene(..)
 , setCamera
 , shade
 ) where

import Control.Lens
import System.Random (RandomGen, uniformR)
import Control.Monad.Trans.State (State, get, put)
import Libs.Camera (Camera, rayToPanel)
import Libs.BVH (BVHAccelerator(..))
import Libs.Spectrum (SpectrumRGB)
import Libs.Intersection (Intersection)
import Libs.Vector (Vector3f, Vector(..), Vector3(..))
import Libs.Ray (Ray(..))
import Libs.Intersectable (Intersectable(..))
import Libs.Intersection (Intersection(..))
import Libs.Material.RenderMaterial (RenderMaterial(..))
import Libs.Object.BaseObject (RenderObject(..))

data Scene = Scene { _camera :: Camera
                   , _bvh :: BVHAccelerator
                   , _spp :: Int
                   } deriving Show
makeLenses ''Scene

setCamera :: Scene -> Camera -> Scene
setCamera s c = camera .~ c $ s

shade :: RandomGen g => Scene -> Int -> Int -> State g SpectrumRGB
shade scene x y = do
  g0 <- get
  let (r1, g1) = uniformR (0, 1) g0
      (r2, g2) = uniformR (0, 1) g1
      (r3, g3) = uniformR (0, 1) g2
      (r4, g4) = uniformR (0, 1) g3
      c = scene ^. camera
      ray = rayToPanel c (x, y) (r1, r2, r3, r4)
      intersection = intersect (scene ^. bvh) ray
  put g4
  shadePixel scene intersection (0 -. getDirection ray)

shadePixel :: RandomGen g => Scene -> Intersection -> Vector3f -> State g SpectrumRGB
shadePixel _ NotIntersect _ = return $ Vector3 0 0 0
shadePixel scene (Intersection co n o) wo =
  if hasEmission $ getMaterial o
  then return . getEmission . getMaterial $ o
  else undefined
