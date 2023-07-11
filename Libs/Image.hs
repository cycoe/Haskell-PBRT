module Libs.Image where

import Data.Char (chr)
import System.FilePath (FilePath)
import System.IO (IOMode(WriteMode), openFile, hPutStr, hClose)
import qualified Data.ByteString as B
import Libs.Spectrum (ScreenRGB)
import qualified Libs.Vector as V

data Image = ImagePPM { getImagePath :: FilePath
                      , get_w :: Int
                      , get_h :: Int
                      } deriving Show

dump :: Image -> [ScreenRGB] -> IO ()
dump (ImagePPM p w h) pixels = do
  let header = "P6\n" ++ show w ++ " " ++ show h ++ "\n255\n"
  handle <- openFile p WriteMode
  hPutStr handle header
  B.hPutStr handle . B.pack . concat $ map V.toList pixels
  hClose handle
