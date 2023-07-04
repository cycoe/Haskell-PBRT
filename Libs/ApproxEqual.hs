module Libs.ApproxEqual
  (ApproxEqual(..)) where

import Libs.Tolerance (Tolerance(..))
import Libs.Vector (Vector3(..))

class ApproxEqual t where
  infixl 4 ~=
  (~=) :: t -> t -> Bool
  infixl 4 ^=
  (^=) :: t -> t -> Bool

instance ApproxEqual Float where
  a ~= b = abs (a - b) < tolerance
  a ^= b =
    let ma = max (abs a) (abs b)
        d  = abs (a - b)
    in if ma == 0 then True else d / ma < tolerance

instance ApproxEqual Double where
  a ~= b = abs (a - b) < tolerance
  a ^= b =
    let ma = max (abs a) (abs b)
        d  = abs (a - b)
    in if ma == 0 then True else d / ma < tolerance

instance ApproxEqual t => ApproxEqual (Vector3 t) where
  Vector3 a1 b1 c1 ~= Vector3 a2 b2 c2 = a1 ~= a2 && b1 ~= b2 && c1 ~= c2
  Vector3 a1 b1 c1 ^= Vector3 a2 b2 c2 = a1 ^= a2 && b1 ^= b2 && c1 ^= c2
