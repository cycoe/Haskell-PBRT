module Libs.Utils where

-- Clamp value by range (a, b)
clamp :: Ord t => t -> t -> t -> t
clamp a b v = max a (min b v)

-- Convert degree angle to rad format
deg2rad :: Floating t => t -> t
deg2rad d = d * pi / 180

-- Floating infinity
infinity :: (Floating t, Read t) => t
infinity = read "Infinity"
