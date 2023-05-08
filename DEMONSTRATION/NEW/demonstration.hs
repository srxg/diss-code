import Lebesgue_MODIF
import Data.Ratio ((%))

main :: IO ()
main = do
  putStrLn "1. Calculate the distance between two points:"
  print $ distance (1 % 2, 3 % 4) (5 % 6, 7 % 8)

  putStrLn "\n2. Calculate the diameter of a shape:"
  let shape = [(0, 0), (1, 1), (1, 0), (0, 1)]
  print $ diameter shape

  putStrLn "\n3. Check if a shape is convex:"
  print $ not (isNotConvex shape)

  putStrLn "\n4. Encode a point as a binary sequence:"
  let point = (2 % 3, 3 % 5)
  print $ encodePoint point

  putStrLn "\n5. Encode a shape as a binary sequence:"
  print $ encodeShape shape

  putStrLn "\n6. Apply transformations to a shape:"
  let piApprox = 22 % 7 -- other approximation can be used if preferred
  let transformation = [Translate (1 % 1, 1 % 1), Rotate (piApprox  / 2) (0, 0)]
  print $ applyTransformations transformation shape

  putStrLn "\n7. Check if a shape is contained in another shape:"
  let shapeA = [(0, 0), (1, 1), (1, 0)]
  let shapeB = [(0, 0), (1, 1), (1, 0), (0, 1)]
  print $ isContained shapeA shapeB

  putStrLn "\n8. Check if a shape is a valid cover of another shape:"
  let transformationList = [[Translate (0, 0)], [Translate (1 % 2, 1 % 2)], [Rotate piApprox  (0, 0), Translate (1 % 1, 1 % 1)]]
  print $ validCover shapeA shapeB transformationList
  
  putStrLn "\n9. Generate points in the Stern-Brocot tree:"
  print $ generatePoints (1 % 4) (3 % 4) (3 % 4)
  
  putStrLn "\n10. Generate the set D of shapes:"
  print $ generateD (1 % 4) (3 % 4) (3 % 4)