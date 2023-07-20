module Libs.Render
  (render) where

import System.Random (RandomGen, newStdGen, split)
import Control.Parallel.Strategies (parList, rseq, rpar, using)
import Control.Monad (foldM_)
import Control.Monad.Trans.State (runState)
import Control.DeepSeq (force)
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Libs.Scene (Scene(..), shade)
import Libs.Camera (Camera(..))
import Libs.Vector (Vector(..), Vector3(..), toList)
import Libs.Spectrum (SpectrumRGB, Spectrum(..))
import Libs.Image (Image(..), dump)
import Libs.ProgressBar (RenderBar, newRenderBar, incRenderProgress)

type Framebuffer = [V.Vector SpectrumRGB]

-- Render a scene to an output file
render :: Scene -> IO ()
render scene = do
  let w = getWidth  . _camera $ scene
      h = getHeight . _camera $ scene
      framebuffer = replicate h $ V.replicate w zero
  pb <- newRenderBar $ _spp scene
  foldM_ (_loopForSPP scene pb) framebuffer [0 .. _spp scene - 1]

_loopForSPP :: Scene -> RenderBar s -> Framebuffer -> Int -> IO (Framebuffer)
_loopForSPP scene pb f spp = do
  gen <- newStdGen
  let (f', bs) = _renderEachSPP scene f gen
      w = getWidth  . _camera $ scene
      h = getHeight . _camera $ scene
      i = ImagePPM "test.ppm" w h
  dump i bs
  incRenderProgress pb
  return f'

-- Render a single spp to framebuffer
_renderEachSPP :: RandomGen g => Scene -> Framebuffer
               -> g -> (Framebuffer, [B.ByteString])
_renderEachSPP scene f gen = (f', bs) where
  rs = zipWith (_renderRow scene) gens rows `using` parList (rpar . force)
  f' = zipWith (V.zipWith (.+.)) f rs `using` parList (rpar . force)
  bs = map toBS f' `using` parList (rpar . force)
  toBS :: V.Vector SpectrumRGB -> B.ByteString
  toBS = B.concat . V.toList . V.map (toByteString . (./ fromIntegral spp))
  spp = _spp scene
  h = getHeight . _camera $ scene
  gens = fst $ _splitGens gen h
  rows = [0 .. h - 1]

_splitGens :: RandomGen g => g -> Int -> ([g], g)
_splitGens g 0 = ([], g)
_splitGens g n = (gr : gs, g2) where
  (gr, g1) = split g
  (gs, g2) = _splitGens g1 (n - 1)

-- Render a row to vector of pixels
_renderRow :: RandomGen g => Scene -> g -> Int -> V.Vector SpectrumRGB
_renderRow scene gen row = pixels where
  w = getWidth . _camera $ scene
  cols = V.enumFromTo 0 (w - 1)
  pixels = fst $ runState (V.mapM (\col -> shade scene col row) cols) gen
