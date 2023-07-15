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
  camera = LensCamera coordinate 768 768 800 0 800
  coordinate = Coordinate position front up
  position = Vector3 278 273 (-800)
  front = Vector3 0 0 1
  up = Vector3 0 1 0
  bvh = buildBVHAccelerator [sphere1, sphere2] BVHNaiveSplit
  emission = Vector3 1 1 1
  kd = Vector3 1 1 1
  light = Diffuse (DiffuseMaterial kd emission)
  white = Diffuse (DiffuseMaterial kd (Vector3 0.1 0.1 0.1))
  sphere1 = SphereObject (Sphere (Vector3 350 100 250) 50 light)
  sphere2 = SphereObject (Sphere (Vector3 150 100 250) 50 white)
  spp = 10
