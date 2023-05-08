import Lebesgue_MODIF
import Data.Ratio ((%))

sampleShapes :: [Shape]
sampleShapes = [
    [(1 % 1, 1 % 1), (1 % 2, 1 % 2)],
    [(1 % 1, 1 % 1), (1 % 1, 2 % 1), (2 % 1, 1 % 1)],
    [(1 % 2, 1 % 2), (1 % 2, 3 % 2), (3 % 2, 1 % 2)]
  ]
  
shapeA = [(1 % 1, 1 % 1), (1 % 2, 1 % 2)]
shapeB = [(1 % 1, 1 % 1), (1 % 1, 2 % 1), (2 % 1, 1 % 1)]

exampleTransformations :: [SpaceOfTransformations]
exampleTransformations = [
    [Translate (0, 0)],
    [Rotate (785398 % 250000) (0, 0), Translate (1, 1)],
    [Rotate 3.14159 (0, 0), Translate (2, 2)]
  ]
  
main :: IO ()
main = do
    let result = validCover shapeA shapeB exampleTransformations
    print result
    --putStrLn "Encoding shapes into binary sequences:"
    --let encodedShapes = map encodeShape sampleShapes
    --mapM_ print encodedShapes
	
{-	
import Lebesgue_MODIF
import Data.Ratio ((%))

shapeA = [(1 % 1, 1 % 1), (1 % 2, 1 % 2)]
shapeB = [(1 % 1, 1 % 1), (1 % 2, 1 % 2), (1 % 1, 2 % 1)]

exampleTransformations :: [SpaceOfTransformations]
exampleTransformations = [
    [Translate (0, 0)],
    [Translate (1 % 2, 1 % 2)],
    [Rotate 3.14159 (0, 0), Translate (1 % 1, 1 % 1)]
  ]

main :: IO ()
main = do
    let result = validCover shapeA shapeB exampleTransformations
    print result
-}