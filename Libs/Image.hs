module Libs.Image where

import System.FilePath (FilePath)
import Data.Vector.Mutable as MV
import Control.Monad.ST (ST)
import Libs.Framebuffer (Framebuffer(..))

data Image = ImagePPM { getImagePath :: FilePath
                      } deriving Show

dump :: Image -> Framebuffer s t -> ST s ()
dump (ImagePPM p) (Framebuffer w h s) = do
  undefined
