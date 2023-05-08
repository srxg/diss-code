import Data.Ratio ((%))
import Lebesgue

sampleShapes :: [Shape]
sampleShapes = [
    [(0, 0), (1, 0), (0, 1)],  -- A right-angled triangle
    [(0, 0), (1, 0), (1, 1), (0, 1)]  -- A square
  ]

halfPiApprox :: Rational
halfPiApprox = 785398 % 250000  -- This is a closer approximation of pi / 2

exampleTransformations :: SpaceOfTransformations
exampleTransformations = [Translate (1, 1), Rotate (halfPiApprox) (0, 0), Flip (1, (0, 0))]

main :: IO ()
main = do
  putStrLn "Original shapes:"
  mapM_ (print . fmap fromRationalPair) sampleShapes
  putStrLn "\nTransformed shapes:"
  let transformedShapes1 = map (applyTransformations exampleTransformations) sampleShapes
  mapM_ print transformedShapes1
  putStrLn "\nTransformed shapes converted to floats (loss in precision):"
  let transformedShapes2 = fmap (applyTransformations exampleTransformations) sampleShapes
  mapM_ (print . fmap fromRationalPair) transformedShapes2
  where
    fromRationalPair (x, y) = (fromRational x, fromRational y)
