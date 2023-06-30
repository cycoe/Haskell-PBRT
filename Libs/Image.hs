module Libs.Image where

import Data.Char (chr)
import System.FilePath (FilePath)
import System.IO (IOMode(WriteMode), openFile, hPutStr, hClose)
import Libs.Spectrum (ScreenRGB)
import qualified Libs.Vector as V

data Image = ImagePPM { getImagePath :: FilePath
                      , get_w :: Int
                      , get_h :: Int
                      } deriving Show

pixelToString :: ScreenRGB -> String
pixelToString pixel = V.toList $ chr <$> pixel

dump :: Image -> [ScreenRGB] -> IO ()
dump (ImagePPM p w h) pixels = do
  let header = "P6\n" ++ show w ++ " " ++ show h ++ "\n255\n"
  handle <- openFile p WriteMode
  hPutStr handle header
  hPutStr handle $ concat $ map pixelToString pixels
  hClose handle
