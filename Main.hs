module Main (main) where

import Options.Applicative
import Libs.Configs (Configs(..))
import Libs.Render (render)
import Libs.Coordinate (Coordinate(..))
import Libs.Camera (Camera(..))
import Libs.Vector (Vector(..), Vector3(..))
import Libs.Scene (Scene(..))
import Libs.Object.Object (Object(..))
import Libs.Object.Sphere (Sphere(..))
import Libs.Object.Triangle (Triangle)
import Libs.Object.TriangleMesh (makeTriangleMesh)
import Libs.Object.ObjLoader (loadObj)
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
main = render =<< _makeScene =<< execParser opts where
  opts = info (configP <**> helper)
    ( fullDesc
   <> header "Physical-based ray tracing, but with Haskell."
    )

_makeScene :: Configs -> IO Scene
_makeScene configs = do
  let camera = LensCamera coordinate 200 200 40 0 800
      coordinate = Coordinate position front up
      position = Vector3 278 273 (-800)
      front = Vector3 0 0 1
      up = Vector3 0 1 0
      emission = 8 *. Vector3 (0.747 + 0.058) (0.747 + 0.258) 0.747 .+.
                 15.6 *. Vector3 (0.740 + 0.287) (0.740 + 0.160) 0.740 .+.
                 18.4 *. Vector3 (0.737 + 0.642) (0.737 + 0.159) 0.737
      light = Material (Diffuse (Vector3 0 0 0) emission)
      red = Material (Diffuse (Vector3 0.63 0.065 0.05) (Vector3 0 0 0))
      green = Material (Diffuse (Vector3 0.14 0.45 0.091) (Vector3 0 0 0))
      white = Material (Diffuse (Vector3 0.725 0.71 0.68) (Vector3 0 0 0))
      mirror = Material (Specular (Vector3 0.9 0.9 0.9) (Vector3 0 0 0))
      sphere1 = Object (Sphere (Vector3 350 150 250) 50 white False)
      sphere2 = Object (Sphere (Vector3 150 150 250) 50 mirror False)
      sphere3 = Object (Sphere (Vector3 250 250 250) 50 light False)
      spp = 128
  floor <- flip makeTriangleMesh white <$> loadObj "./Models/cornellbox/floor.obj"
  left <- flip makeTriangleMesh red <$> loadObj "./Models/cornellbox/left.obj"
  right <- flip makeTriangleMesh green <$> loadObj "./Models/cornellbox/right.obj"
  tall <- flip makeTriangleMesh white <$> loadObj "./Models/cornellbox/tallbox.obj"
  short <- flip makeTriangleMesh white <$> loadObj "./Models/cornellbox/shortbox.obj"
  l <- flip makeTriangleMesh light <$> loadObj "./Models/cornellbox/light.obj"
  let objects = [ Object floor, Object left, Object right
                , Object tall, Object short, Object l]
      scene = Scene camera bvh spp
      bvh = buildBVHAccelerator objects BVHNaiveSplit
  return scene
