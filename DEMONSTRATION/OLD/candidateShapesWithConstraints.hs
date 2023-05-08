import Lebesgue
import Data.Ratio ((%))

main :: IO ()
main = do
    putStrLn "Generated candidate shapes with minX = 0, maxX = 1, maxY = 1:"
    let candidateShapes = generateD 0 1 1
    mapM_ print candidateShapes