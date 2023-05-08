import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

newtype Point = Point (Rational, Rational)
type Shape = [Point]

class MetricSpace a where
  distance :: a -> a -> Rational
  
instance MetricSpace Point where
  distance (Point (x1, y1)) (Point (x2, y2)) = toRational $ sqrt $ fromRational ((x2 - x1)^2 + (y2 - y1)^2)

directedHausdorff :: Shape -> Shape -> Rational
directedHausdorff shape1 shape2 = maximum $ map (\point1 -> minimum $ map (distance point1) shape2) shape1

instance MetricSpace Shape where
  distance shape1 shape2 = max (directedHausdorff shape1 shape2) (directedHausdorff shape2 shape1)
