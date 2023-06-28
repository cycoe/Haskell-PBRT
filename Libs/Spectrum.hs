module Libs.Spectrum where

import qualified Libs.Vector as V

-- Spectrum described by RGB
type SpectrumRGB = V.Vector3f

-- Screnn pixel described by RGB
type ScreenRGB = V.Vector3 Int

class Spectrum s where
  to_screen_rgb :: s -> ScreenRGB

instance Spectrum SpectrumRGB where
  to_screen_rgb v = floor <$> 255 V.*. V.pow (V.clamp 0 1 v) 0.6
