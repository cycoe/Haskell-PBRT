module Libs.Camera where

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
                         } deriving Show

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
