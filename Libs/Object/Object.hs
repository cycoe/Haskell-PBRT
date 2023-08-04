{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE InstanceSigs#-}
{-#LANGUAGE ExistentialQuantification#-}
module Libs.Object.Object
  ( Object(..)
  , RenderObject(..)
  , Intersectable(..)
  ) where

import System.Random (RandomGen)
import Control.Monad.Trans.State (State)
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import Data.Maybe (isJust)

import Libs.Vector (Vector3f)
import Libs.Bounds3 (Bounds3)
import Libs.Material.Material (Material)
import Libs.Intersection (Intersection)
import Libs.Ray (Ray)

data Object = forall a. ( Show a
                        , Generic a
                        , NFData a
                        , RenderObject a
                        , Intersectable a
                        ) => Object a

instance Generic Object

-- | Enable evaluated to NFData
instance NFData Object where
  rnf (Object o) = rnf o

instance Show Object where
  show (Object o) = show o

-- Renderable object class
class RenderObject ro where
  getObjectBounds :: ro -> Bounds3
  getMaterial :: ro -> Material
  -- sample on object returns a tuple of intersect point and pdf
  sample :: RandomGen g => ro -> State g (Intersection Object, Float)
  getArea :: ro -> Float
  getLocalCS :: ro -> Vector3f -> (Vector3f, Vector3f, Vector3f)

instance RenderObject Object where
  getObjectBounds (Object o) = getObjectBounds o
  getMaterial (Object o) = getMaterial o
  sample (Object o) = sample o
  getArea (Object o) = getArea o
  getLocalCS (Object o) p = getLocalCS o p

class Intersectable c where
  intersect :: c -> Ray -> Maybe (Intersection Object)
  intersectP :: c -> Ray -> Bool
  intersectP c ray = isJust $ intersect c ray

instance Intersectable Object where
  intersect :: Object -> Ray -> Maybe (Intersection Object)
  intersect (Object o) ray = intersect o ray
