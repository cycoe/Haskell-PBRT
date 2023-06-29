module Libs.Framebuffer where

import Data.Vector.Mutable as MV
import Control.Monad.ST (ST)
import Libs.Spectrum (Spectrum, zero)

-- Framebuffer stands for a vector of some Spectrum pixels
-- type s -> state of mutable vector
-- type t -> element type of framebuffer
-- param get_w -> width of framebuffer
-- param get_h -> height of framebuffer
-- param get_state -> mutable vector
data Framebuffer s t = Framebuffer { get_w :: Int
                                   , get_h :: Int
                                   , get_state :: ST s (MV.MVector s t)
                                   }

-- create a framebuffer initialized by spectrum initialization function
-- param w -> width of framebuffer
-- param h -> height of framebuffer
create_framebuffer :: Spectrum t => Int -> Int -> Framebuffer s t
create_framebuffer w h = Framebuffer w h $ do
  buffer <- MV.replicate (w * h) zero
  return buffer

-- update [x, y] pixel in framebuffer
-- param x -> coordinate x of pixel
-- param y -> coordinate y of pixel
-- param s -> new pixel spectrum
update_pixel :: Framebuffer s t -> Int -> Int -> t -> Framebuffer s t
update_pixel (Framebuffer w h state) x y s = Framebuffer w h $ do
  buffer <- state
  MV.write buffer (x + y * w) s
  return buffer

-- get [x, y] pixel in framebuffer
-- param x -> coordinate x of pixel
-- param y -> coordinate y of pixel 
get_pixel :: Framebuffer s t -> Int -> Int -> ST s t
get_pixel (Framebuffer w h state) x y = do
  buffer <- state
  MV.read buffer (x + y * w)
