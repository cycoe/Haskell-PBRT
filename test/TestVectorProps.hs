module TestVectorProps (testVectorProps) where

import Test.Hspec (hspec)
import Test.Hspec.QuickCheck (prop)
import Libs.Tuple (Tuple3)
import Libs.Vector (Vector(..), Vector3(..), norm, normalize)
import Libs.ApproxEqual (ApproxEqual(..))

-- Convert a tuple to vector, because tuple of Num is the instance of Arbitrary,
-- so we don't need to implement generator by ourselves
tupleToVector :: Tuple3 t -> Vector3 t
tupleToVector (a, b, c) = Vector3 a b c

type VectorWithVector t = Vector3 t -> Vector3 t -> Vector3 t

testVectorBinarySwap :: VectorWithVector Float -> Tuple3 Float -> Tuple3 Float -> Bool
testVectorBinarySwap op v1 v2 = (tupleToVector v1 `op` tupleToVector v2) ==
                                (tupleToVector v2 `op` tupleToVector v1)

testVectorBinaryCombine :: VectorWithVector Float -> Tuple3 Float -> Tuple3 Float
                        -> Tuple3 Float -> Bool
testVectorBinaryCombine op v1 v2 v3 =
  ((tupleToVector v1 `op` tupleToVector v2) `op` tupleToVector v3) ^=
  (tupleToVector v1 `op` (tupleToVector v2 `op` tupleToVector v3))

testVectorAddZero :: Tuple3 Float -> Bool
testVectorAddZero v = tupleToVector v .+. Vector3 0 0 0 == tupleToVector v

testVectorMinusZero :: Tuple3 Float -> Bool
testVectorMinusZero v = tupleToVector v .-. Vector3 0 0 0 == tupleToVector v

testVectorMultiplyZero :: Tuple3 Float -> Bool
testVectorMultiplyZero v = tupleToVector v .*. Vector3 0 0 0 == Vector3 0 0 0

testVectorMultiplyOne :: Tuple3 Float -> Bool
testVectorMultiplyOne v = tupleToVector v .*. Vector3 1 1 1 == tupleToVector v

testVectorDivideOne :: Tuple3 Float -> Bool
testVectorDivideOne v = tupleToVector v ./. Vector3 1 1 1 == tupleToVector v

testVectorScaleNorm :: Tuple3 Float -> Float -> Bool
testVectorScaleNorm v s = norm (s *. tupleToVector v) ^= abs s * norm (tupleToVector v)

testVectorNormalizedNorm :: Tuple3 Float -> Bool
testVectorNormalizedNorm v = norm (normalize $ tupleToVector v) ^= 1

testVectorProps :: [IO ()]
testVectorProps = hspec <$>
  [ prop "testVectorAddSwap" $ testVectorBinarySwap (.+.)
  , prop "testVectorMultiplySwap" $ testVectorBinarySwap (.*.)
  , prop "testVectorAddCombine" $ testVectorBinaryCombine (.+.)
  , prop "testVectorMultiplyCombine" $ testVectorBinaryCombine (.*.)
  , prop "testVectorAddZero" testVectorAddZero
  , prop "testVectorMinusZero" testVectorMinusZero
  , prop "testVectorMultiplyZero" testVectorMultiplyZero
  , prop "testVectorMultiplyOne" testVectorMultiplyOne
  , prop "testVectorDivideOne" testVectorDivideOne
  , prop "testVectorScaleNorm" testVectorScaleNorm
  , prop "testVectorNormalizedNorm" testVectorNormalizedNorm
  ]
