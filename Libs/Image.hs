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

dump :: Image -> [B.ByteString] -> IO ()
dump (ImagePPM p w h) rows = do
  let header = "P6\n" ++ show w ++ " " ++ show h ++ "\n255\n"
  handle <- openFile p WriteMode
  hPutStr handle header
  mapM_ (B.hPutStr handle) rows
  hClose handle
