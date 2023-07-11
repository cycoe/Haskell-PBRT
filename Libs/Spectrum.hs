{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Libs.Spectrum where

import qualified Libs.Vector as V
import Data.Word (Word8)

-- Spectrum described by RGB
type SpectrumRGB = V.Vector3 Float

-- Screnn pixel described by RGB
type ScreenRGB = V.Vector3 Word8

class Spectrum s where
  zero :: s
  to_screen_rgb :: s -> ScreenRGB

instance Spectrum SpectrumRGB where
  zero = V.Vector3 0 0 0
  to_screen_rgb v = floor <$> 255 V.*. V.pow (V.clamp 0 1 v) 0.6
