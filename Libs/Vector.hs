module Libs.Vector where

class Vector v where
  vector_with_vector :: (t -> t -> t) -> v t -> v t -> v t
  vector_with_scalar :: (t -> t -> t) -> v t -> t -> v t
  scalar_with_vector :: (t -> t -> t) -> t -> v t -> v t
  (.+.) :: Num t => v t -> v t -> v t
  (.+.) = vector_with_vector (Prelude.+)
  (.-.) :: Num t => v t -> v t -> v t
  (.-.) = vector_with_vector (Prelude.-)
  (.*.) :: Num t => v t -> v t -> v t
  (.*.) = vector_with_vector (Prelude.*)
  (./.) :: Fractional t => v t -> v t -> v t
  (./.) = vector_with_vector (Prelude./)
  (.+) :: Num t => v t -> t -> v t
  (.+) = vector_with_scalar (Prelude.+)
  (.-) :: Num t => v t -> t -> v t
  (.-) = vector_with_scalar (Prelude.-)
  (.*) :: Num t => v t -> t -> v t
  (.*) = vector_with_scalar (Prelude.*)
  (./) :: Fractional t => v t -> t -> v t
  (./) = vector_with_scalar (Prelude./)
  (+.) :: Num t => t -> v t -> v t
  (+.) = scalar_with_vector (Prelude.+)
  (-.) :: Num t => t -> v t -> v t
  (-.) = scalar_with_vector (Prelude.-)
  (*.) :: Num t => t -> v t -> v t
  (*.) = scalar_with_vector (Prelude.*)
  (/.) :: Fractional t => t -> v t -> v t
  (/.) = scalar_with_vector (Prelude./)

data Vector3 t = Vector3 { x :: {-# UNPACK #-} !t
                         , y :: {-# UNPACK #-} !t
                         , z :: {-# UNPACK #-} !t
                         }

instance Show t => Show (Vector3 t) where
  show (Vector3 a b c) = "[" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ "]"

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
