{-#LANGUAGE TemplateHaskell #-}
module Libs.Scene where

import Control.Lens
import System.Random (RandomGen, uniformR)
import Control.Monad.Trans.State (State, get, put)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Libs.Camera (Camera, rayToPanel)
import Libs.BVH (BVHAccelerator(..), getObjects)
import Libs.Spectrum (SpectrumRGB, zero)
import Libs.Intersection (Intersection)
import Libs.Vector (Vector3f, Vector(..), Vector3(..), normalize, dot, norm)
import Libs.Ray (Ray(..))
import Libs.Intersectable (Intersectable(..))
import Libs.Intersection (Intersection(..))
import Libs.Material.Material (worldToLocal, localToWorld)
import Libs.Material.RenderMaterial (RenderMaterial(..))
import Libs.Object.BaseObject (RenderObject(..))
import Libs.Utils (sumFromHere)

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

shadePixel :: RandomGen g => Scene -> Maybe Intersection
           -> Vector3f -> State g SpectrumRGB
shadePixel _ Nothing _ = return $ Vector3 0 0 0
shadePixel scene (Just i@(Intersection co n o)) wo =
  if hasEmission $ getMaterial o
  then return . getEmission . getMaterial $ o
  -- TODO: indirect illuminate
  else (.+.)
       <$> directIlluminate scene i wo
       <*> indirectIlluminate scene i wo

directIlluminate :: RandomGen g => Scene -> Intersection
                 -> Vector3f -> State g SpectrumRGB
directIlluminate scene (Intersection co n o) wo = do
  (hitLight, pdfLight) <- sampleLight scene
  let localCS = getLocalCS o co
      localWo = worldToLocal wo localCS
      shadeToLight = getCoordinate hitLight .-. co
      shadeToLightDir = normalize shadeToLight
      localShadeToLight = worldToLocal shadeToLightDir localCS
      rayToLight = Ray co shadeToLightDir
      hitMask = fromMaybe hitLight $ intersect (scene ^. bvh) rayToLight
      shadeToMask = getCoordinate hitMask .-. co
      material = getMaterial o
      fr = eval material localShadeToLight localWo
      r2 = dot shadeToLight shadeToLight
      cosa = max 0 $ dot n shadeToLightDir
      cosb = max 0 $ dot (getNormal hitLight) (1 -. shadeToLightDir)
      emission = getEmission . getMaterial . getObject $ hitLight
  if norm shadeToMask - norm shadeToLight < -0.0001
  then return $ Vector3 0 0 0
  else return $ cosa * cosb / r2 / pdfLight *. fr .*. emission

indirectIlluminate :: RandomGen g => Scene -> Intersection
                   -> Vector3f -> State g SpectrumRGB
indirectIlluminate scene (Intersection co n o) wo = do
  g0 <- get
  let (p, g1) = uniformR (0, 1) g0
  put g1
  if p > (0.8 :: Float)
  then return zero
  else do
    let material = getMaterial o
        localCS = getLocalCS o co
        localWo = worldToLocal wo localCS
    localWi <- Libs.Material.RenderMaterial.sample material localWo
    let _pdf = pdf material localWo localWi
        wi = localToWorld localWi localCS
        rayToNext = Ray co wi
        hitNext = intersect (scene ^. bvh) rayToNext
        fr = eval material localWi localWo
        coswi = max 0 $ dot n wi
        nextWo = (0 -. wi)
    if _pdf == 0 then return zero else case hitNext of
      Nothing -> return zero
      Just (Intersection _ _ nextObj) ->
        if hasEmission $ getMaterial nextObj
        then return $ Vector3 0 0 0
        else (coswi / _pdf / 0.8 *. fr .*.) <$> shadePixel scene hitNext nextWo

sampleLight :: RandomGen g => Scene -> State g (Intersection, Float)
sampleLight scene = do
  gen <- get
  -- TODO: implement more efficeint traverse and areaSum method
  let lights = filter (hasEmission . getMaterial) $ getObjects (_bvh scene)
      areas = getArea <$> lights
      areaAccuml = sumFromHere areas
      -- TODO: ensure objects not null
      areaSum = last areaAccuml
      (p, gen') = uniformR (0, areaSum) gen
      light = case find ((>) p . fst) $ zip areaAccuml lights of
        Nothing     -> last lights
        Just (_, o) -> o
  put gen'
  Libs.Object.BaseObject.sample light
