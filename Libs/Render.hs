module Libs.Render
  (render) where

import System.Random (RandomGen, newStdGen)
import Control.Parallel.Strategies (parMap, rpar)
import Control.Monad (foldM_)
import Control.Monad.Trans.State (runState)
import qualified Data.Vector as V
import Libs.Scene (Scene(..), shade)
import Libs.Framebuffer (Framebuffer(..), create_framebuffer)
import Libs.Camera (Camera(..))
import Libs.Vector (Vector(..), Vector3(..))
import Libs.Spectrum (SpectrumRGB, Spectrum(..))
import Libs.Image (Image(..), dump)

-- Render a scene to an output file
render :: Scene -> IO ()
render scene = do
  let w = getWidth  . _camera $ scene
      h = getHeight . _camera $ scene
      framebuffer = create_framebuffer w h :: Framebuffer SpectrumRGB
  foldM_ (_loopForSPP scene) framebuffer [0 .. _spp scene - 1]

_loopForSPP :: Scene -> Framebuffer SpectrumRGB -> Int -> IO (Framebuffer SpectrumRGB)
_loopForSPP scene (Framebuffer w h f) spp = do
  gen <- newStdGen
  let v = _renderEachSPP scene gen
      f' = V.zipWith (.+.) f v
      p = V.toList $ V.map (to_screen_rgb . (./ fromIntegral spp)) f'
      i = ImagePPM "test.ppm" w h
  dump i p
  return $ Framebuffer w h f'

-- Render a single spp to framebuffer
_renderEachSPP :: RandomGen g => Scene -> g -> V.Vector SpectrumRGB
_renderEachSPP scene gen = V.concat $ parMap rpar (_renderRow scene gen) rows where
  w = getWidth  . _camera $ scene
  h = getHeight . _camera $ scene
  rows = [0 .. h - 1]

-- Render a row to vector of pixels
_renderRow :: RandomGen g => Scene -> g -> Int -> V.Vector SpectrumRGB
_renderRow scene gen row = pixels where
  w = getWidth . _camera $ scene
  indexes = V.enumFromTo 0 (w - 1)
  (pixels, _) = runState (V.mapM (shade scene row) indexes) gen
