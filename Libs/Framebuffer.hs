module Libs.Framebuffer where

import Data.Vector as V
import Libs.Spectrum (Spectrum, zero)

-- Framebuffer stands for a vector of some Spectrum pixels
-- type s -> state of mutable vector
-- type t -> element type of framebuffer
-- param get_w -> width of framebuffer
-- param get_h -> height of framebuffer
-- param get_state -> mutable vector
data Framebuffer t = Framebuffer { get_w :: Int
                                 , get_h :: Int
                                 , get_f :: V.Vector t
                                 }

-- create a framebuffer initialized by spectrum initialization function
-- param w -> width of framebuffer
-- param h -> height of framebuffer
create_framebuffer :: Spectrum t => Int -> Int -> Framebuffer t
create_framebuffer w h = Framebuffer w h $ V.replicate (w * h) zero
