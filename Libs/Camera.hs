{-#LANGUAGE DeriveGeneric#-}
module Libs.Camera where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Libs.Coordinate (Coordinate(..), getRight)
import Libs.Ray (Ray(..))
import Libs.Utils (deg2rad)
import Libs.Vector (Vector3(Vector3), (.+.), (.-.),  (*.), normalize)
import Libs.Tuple (Tuple2, Tuple4)

data Camera = LensCamera { getCoordinate :: Coordinate
                         , getWidth :: Int
                         , getHeight :: Int
                         , getFOV :: Float
                         , getAperture :: Float
                         , getFocalLength :: Float
                         } deriving (Show, Generic)

-- | Enable evaluated to NFData
instance NFData Camera

-- Cast a ray from camera to render panel
-- param Camera -> camera cast from
-- param Tuple2 Int -> pixel indices on render panel
-- param Tuple4 Float -> 4 random values used to determine the ray
rayToPanel :: Camera -> Tuple2 Int -> Tuple4 Float -> Ray
rayToPanel (LensCamera c w h fov ap fl) (x, y) (r1, r2, r3, r4) =
  let scale = tan . deg2rad $ fov * 0.5
      aspectRatio = fromIntegral w / fromIntegral h
      pixelOffsetH = (2 * (fromIntegral x + r1) / fromIntegral w - 1) * aspectRatio * scale
      pixelOffsetV = (1 - 2 * (fromIntegral y + r2) / fromIntegral h) * scale
      lensOffsetH = (2 * r3 - 1) * ap
      lensOffsetV = (2 * r4 - 1) * ap
      front = getFront c
      right = getRight c
      up = getUp c
      dir2Pixel = front .+. pixelOffsetH *. right .+. pixelOffsetV *. up
      focalPoint = fl *. normalize dir2Pixel
      lensPoint = lensOffsetH *. right .+. lensOffsetV *. up
  in Ray (getPosition c) (normalize $ focalPoint .-. lensPoint)
