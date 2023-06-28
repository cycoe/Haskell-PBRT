module Libs.Utils where

clamp :: Ord t => t -> t -> t -> t
clamp a b v = max a (min b v)
