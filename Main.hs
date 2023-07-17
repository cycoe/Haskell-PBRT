module Main (main) where

import Options.Applicative
import Libs.Configs (Configs(..))
import Libs.Render (render)
import Libs.Coordinate (Coordinate(..))
import Libs.Camera (Camera(..))
import Libs.Vector (Vector3(..))
import Libs.Scene (Scene(..))
import Libs.Object.Object (Object(..))
import Libs.Object.Sphere (Sphere(..))
import Libs.Material.Material (Material(..))
import Libs.Material.Diffuse (DiffuseMaterial(..))
import Libs.Spectrum (SpectrumRGB)
import Libs.BVH (buildBVHAccelerator, BVHSplitMethod(..))

configP :: Parser Configs
configP = Configs
  <$> switch ( long "quick-check"
            <> short 'q'
            <> help "Quick check rendering result with low resolutions and spp"
            <> showDefault
             )

main :: IO ()
main = render . _makeScene =<< execParser opts where
  opts = info (configP <**> helper)
    ( fullDesc
   <> header "Physical-based ray tracing, but with Haskell."
    )

_makeScene :: Configs -> Scene
_makeScene configs = scene where
  scene = Scene camera bvh spp
  camera = LensCamera coordinate 200 200 90 0 800
  coordinate = Coordinate position front up
  position = Vector3 278 273 0
  front = Vector3 0 0 1
  up = Vector3 0 1 0
  bvh = buildBVHAccelerator objects BVHNaiveSplit
  emission = Vector3 20 15 10
  light = Diffuse (DiffuseMaterial (Vector3 0 0 0) emission)
  red = Diffuse (DiffuseMaterial (Vector3 0.9 0.7 0.3) (Vector3 0 0 0))
  blue = Diffuse (DiffuseMaterial (Vector3 0.1 0.3 0.7) (Vector3 0 0 0))
  grey = Diffuse (DiffuseMaterial (Vector3 0.3 0.3 0.3) (Vector3 0 0 0))
  sphere1 = SphereObject (Sphere (Vector3 350 250 250) 50 light False)
  sphere2 = SphereObject (Sphere (Vector3 150 250 250) 50 red False)
  sphere3 = SphereObject (Sphere (Vector3 250 350 250) 50 blue False)
  sphere4 = SphereObject (Sphere (Vector3 250 250 250) 300 grey True)
  objects = [sphere1, sphere2, sphere3, sphere4]
  spp = 100
