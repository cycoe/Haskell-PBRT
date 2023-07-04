module Libs.Tolerance
  (Tolerance(..)) where

class Tolerance t where
  tolerance :: t

instance Tolerance Float where
  tolerance = 1e-4

instance Tolerance Double where
  tolerance = 1e-8
