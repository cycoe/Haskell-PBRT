{-#LANGUAGE TemplateHaskell #-}
module Libs.Scene
 ( Scene(..)
 , setCamera
 , addObject
 , addObjects
 , shade
 ) where

import Control.Lens
import System.Random (RandomGen)
import Control.Monad.Trans.State (State)
import Libs.Camera (Camera)
import Libs.Object.Object (Object)
import Libs.Spectrum (SpectrumRGB)

data Scene = Scene { _camera :: Camera
                   , _objects :: [Object]
                   } deriving Show
makeLenses ''Scene

setCamera :: Scene -> Camera -> Scene
setCamera s c = camera .~ c $ s

addObject :: Scene -> Object -> Scene
addObject s o = objects %~ (o:) $ s

addObjects :: Scene -> [Object] -> Scene
addObjects s os = objects %~ (++os) $ s

shade :: RandomGen g =>  Scene -> State g SpectrumRGB
shade = undefined
