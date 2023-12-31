{-#LANGUAGE DeriveGeneric#-}
module Libs.Coordinate where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Libs.Vector (Vector3, cross)

-- Coordinate stands for a specific position and orientation in space.
-- Use (position, front, up) to describe an unique state
data Coordinate = Coordinate { getPosition :: Vector3 Float
                             , getFront :: Vector3 Float
                             , getUp :: Vector3 Float
                             } deriving (Show, Generic, Eq)

-- | Enable evaluated to NFData
instance NFData Coordinate

-- Right orientation can get by front X up
getRight :: Coordinate -> Vector3 Float
getRight (Coordinate _ front up) = cross front up
