module Libs.Utils where

clamp :: Ord t => t -> t -> t -> t
clamp a b v = max a (min b v)

deg2rad :: Floating t => t -> t
deg2rad d = d * pi / 180

infinity :: (Floating t, Read t) => t
infinity = read "Infinity"
