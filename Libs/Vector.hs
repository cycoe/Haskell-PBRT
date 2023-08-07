{-#LANGUAGE DeriveGeneric#-}
module Libs.Vector where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Libs.Utils as U

class Vector v where
  vector_with_vector :: (t -> t -> t) -> v t -> v t -> v t
  vector_with_scalar :: (t -> t -> t) -> v t -> t -> v t
  scalar_with_vector :: (t -> t -> t) -> t -> v t -> v t
  infixl 6 .+.
  (.+.) :: Num t => v t -> v t -> v t
  (.+.) = vector_with_vector (Prelude.+)
  infixl 6 .-.
  (.-.) :: Num t => v t -> v t -> v t
  (.-.) = vector_with_vector (Prelude.-)
  infixl 7 .*.
  (.*.) :: Num t => v t -> v t -> v t
  (.*.) = vector_with_vector (Prelude.*)
  infixl 7 ./.
  (./.) :: Fractional t => v t -> v t -> v t
  (./.) = vector_with_vector (Prelude./)
  infixl 6 .+
  (.+) :: Num t => v t -> t -> v t
  (.+) = vector_with_scalar (Prelude.+)
  infixl 6 .-
  (.-) :: Num t => v t -> t -> v t
  (.-) = vector_with_scalar (Prelude.-)
  infixl 7 .*
  (.*) :: Num t => v t -> t -> v t
  (.*) = vector_with_scalar (Prelude.*)
  infixl 7 ./
  (./) :: Fractional t => v t -> t -> v t
  (./) = vector_with_scalar (Prelude./)
  infixl 6 +.
  (+.) :: Num t => t -> v t -> v t
  (+.) = scalar_with_vector (Prelude.+)
  infixl 6 -.
  (-.) :: Num t => t -> v t -> v t
  (-.) = scalar_with_vector (Prelude.-)
  infixl 7 *.
  (*.) :: Num t => t -> v t -> v t
  (*.) = scalar_with_vector (Prelude.*)
  infixl 7 /.
  (/.) :: Fractional t => t -> v t -> v t
  (/.) = scalar_with_vector (Prelude./)

data Vector3 t = Vector3 { x :: {-# UNPACK #-} !t
                         , y :: {-# UNPACK #-} !t
                         , z :: {-# UNPACK #-} !t
                         } deriving (Eq, Generic)

type Vector3f = Vector3 Float

instance NFData t => NFData (Vector3 t)

instance Show t => Show (Vector3 t) where
  show (Vector3 a b c) = "[" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ "]"

instance Functor Vector3 where
  fmap f (Vector3 a b c) = Vector3 (f a) (f b) (f c)

instance Vector Vector3 where
  vector_with_vector op (Vector3 a1 b1 c1) (Vector3 a2 b2 c2) =
    Vector3 (op a1 a2) (op b1 b2) (op c1 c2)
  vector_with_scalar op (Vector3 a b c) s =
    Vector3 (op a s) (op b s) (op c s)
  scalar_with_vector op s (Vector3 a b c) =
    Vector3 (op s a) (op s b) (op s c)

norm :: Floating t => Vector3 t -> t
norm (Vector3 a b c) = sqrt $ a * a + b * b + c * c

normalize :: Floating t => Vector3 t -> Vector3 t
normalize v = v ./ norm v

dot :: Num t => Vector3 t -> Vector3 t -> t
dot (Vector3 a1 b1 c1) (Vector3 a2 b2 c2) = a1 * a2 + b1 * b2 + c1 * c2

cross :: Num t => Vector3 t -> Vector3 t -> Vector3 t
cross (Vector3 a1 b1 c1) (Vector3 a2 b2 c2) =
  Vector3
  (b1 * c2 - c1 * b2)
  (c1 * a2 - a1 * c2)
  (a1 * b2 - b1 * a2)

clamp :: Ord t => t -> t -> Vector3 t -> Vector3 t
clamp m n v = U.clamp m n <$> v

pow :: Floating t => Vector3 t -> t -> Vector3 t
pow v m = (**m) <$> v

toList :: Vector3 t -> [t]
toList (Vector3 a b c) = [a, b, c]

vmin :: Ord t => Vector3 t -> Vector3 t -> Vector3 t
vmin (Vector3 a1 b1 c1) (Vector3 a2 b2 c2) = Vector3
  (Prelude.min a1 a2)
  (Prelude.min b1 b2)
  (Prelude.min c1 c2)

vmax :: Ord t => Vector3 t -> Vector3 t -> Vector3 t
vmax (Vector3 a1 b1 c1) (Vector3 a2 b2 c2) = Vector3
  (Prelude.max a1 a2)
  (Prelude.max b1 b2)
  (Prelude.max c1 c2)

vsqrt :: Floating t => Vector3 t -> Vector3 t
vsqrt (Vector3 a b c) = Vector3 (sqrt a) (sqrt b) (sqrt c)
