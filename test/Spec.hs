import TestVectorProps

main :: IO ()
main = do
  sequence_ testVectorProps
  return ()
