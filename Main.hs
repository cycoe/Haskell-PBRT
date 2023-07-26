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
import Libs.Object.Triangle (Triangle, makeTriangle)
import Libs.Material.Material (Material(..))
import Libs.Material.Diffuse (Diffuse(..))
import Libs.Material.Specular (Specular(..))
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
  camera = LensCamera coordinate 200 200 40 0 800
  coordinate = Coordinate position front up
  position = Vector3 278 273 (-800)
  front = Vector3 0 0 1
  up = Vector3 0 1 0
  bvh = buildBVHAccelerator objects BVHNaiveSplit
  emission = Vector3 20 15 10
  light = Material (Diffuse (Vector3 0 0 0) emission)
  red = Material (Diffuse (Vector3 0.9 0.4 0.3) (Vector3 0 0 0))
  blue = Material (Diffuse (Vector3 0.1 0.3 0.7) (Vector3 0 0 0))
  white = Material (Diffuse (Vector3 0.75 0.7 0.5) (Vector3 0 0 0))
  mirror = Material (Specular (Vector3 0.9 0.9 0.9) (Vector3 0 0 0))
  sphere1 = Object (Sphere (Vector3 350 150 250) 50 white False)
  sphere2 = Object (Sphere (Vector3 150 150 250) 50 mirror False)
  sphere3 = Object (Sphere (Vector3 250 250 250) 50 light False)
  a1 = Vector3 500 500 0
  a2 = Vector3 0 500 0
  a3 = Vector3 0 500 500
  a4 = Vector3 500 500 500
  b1 = Vector3 500 0 0
  b2 = Vector3 0 0 0
  b3 = Vector3 0 0 500
  b4 = Vector3 500 0 500
  l1 = Vector3 300 499 200
  l2 = Vector3 200 499 200
  l3 = Vector3 200 499 300
  l4 = Vector3 300 499 300
  floor1 = Object (makeTriangle b1 b2 b3 Nothing white)
  floor2 = Object (makeTriangle b1 b3 b4 Nothing white)
  back1 = Object (makeTriangle b4 b3 a3 Nothing white)
  back2 = Object (makeTriangle b4 a3 a4 Nothing white)
  left1 = Object (makeTriangle b1 b4 a4 Nothing red)
  left2 = Object (makeTriangle b1 a4 a1 Nothing red)
  right1 = Object (makeTriangle b2 a2 a3 Nothing blue)
  right2 = Object (makeTriangle b2 a3 b3 Nothing blue)
  top1 = Object (makeTriangle a1 a3 a2 Nothing white)
  top2 = Object (makeTriangle a1 a4 a3 Nothing white)
  light1 = Object (makeTriangle l1 l3 l2 Nothing light)
  light2 = Object (makeTriangle l1 l4 l3 Nothing light)
  objects = [sphere1, sphere2, sphere3, light1, light2,
             floor1, floor2, back1, back2, left1, left2, right1, right2, top1, top2]
  spp = 10
