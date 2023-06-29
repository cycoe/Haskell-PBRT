module Libs.Framebuffer where

import Libs.Vector as V
import Data.Vector.Mutable as MV
import Control.Monad.ST (ST)

data Framebuffer s = Framebuffer { get_w :: Int
                                 , get_h :: Int
                                 , get_state :: ST s (MV.MVector s V.Vector3f)
                                 }

create_framebuffer :: Int -> Int -> Framebuffer s
create_framebuffer w h = Framebuffer w h $ do
  buffer <- MV.replicate (w * h) (V.Vector3 0 0 0)
  return buffer

update_pixel :: Framebuffer s -> Int -> Int -> Vector3f -> Framebuffer s
update_pixel (Framebuffer w h state) x y v = Framebuffer w h $ do
  buffer <- state
  MV.write buffer (x + y * w) v
  return buffer

get_pixel :: Framebuffer s -> Int -> Int -> ST s Vector3f
get_pixel (Framebuffer w h state) x y = do
  buffer <- state
  MV.read buffer (x + y * w)
