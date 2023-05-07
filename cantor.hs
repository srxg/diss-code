import Control.Monad
import Data.List (nub, maximumBy, minimumBy, transpose)
import Data.Function (on)
import Data.Ratio ((%))


type Point = (Rational, Rational)
type Shape = [Point]

-- Generate the Stern-Brocot tree
data Tree a = Node a (Tree a) (Tree a)

sternBrocot :: Tree Rational
sternBrocot = build 0 1 1 0
  where
    build l1 l2 r1 r2 = Node m (build l1 l2 m1 m2) (build m1 m2 r1 r2)
      where
        m1 = l1 + r1
        m2 = l2 + r2
        m = m1 % m2

-- Convert a rational number to its binary representation in the Stern-Brocot tree
rationalToBinary :: Rational -> [Integer]
rationalToBinary r = go sternBrocot
  where
    go (Node x left right)
      | r == x = []
      | r < x = 0 : go left
      | otherwise = 1 : go right

-- Encode a point as a binary sequence
encodePoint :: Point -> [Integer]
encodePoint (x, y) = interleave (rationalToBinary x) (rationalToBinary y)
  where
    interleave xs ys = concat $ transpose [xs, ys]

-- Encode a shape as a binary sequence
encodeShape :: Shape -> [Integer]
encodeShape = concatMap encodePoint

-- Cantor space representation of convex, diameter 1 sets

-- Cantor space representation of convex, diameter 1 sets
gridPoints :: [Point]
gridPoints = [(x, y) | x <- rationalList, y <- rationalList]
  where
    rationalList = [0, 1 % 2] ++ [1 % n | n <- [3..]]


pythag :: Point -> Point -> Rational
pythag (x1,y1) (x2,y2) = sqrt ((abs(x2-x1)**2.0) + (abs(y2-y1)**2.0))

distance :: Point -> Point -> Rational
distance (x1, y1) (x2, y2) = if (x1,y1) == (x2,y2) then 0.0 else pythag (x1,y1) (x2,y2)


cartesianProd :: Shape -> Shape -> [(Point,Point)]
cartesianProd s1 s2 = [(p,q) | p <- s1, q <- s2]

diameter :: Shape -> Rational
diameter s = if null s then 0.0 else maximum [distance i j | (i,j) <- cartesianProd s s]


-- this is made as a helper for the isNotConvex function below
-- the idea here is to use the z-component of the cross product
-- of the 3d versions of the 2d vectors to determine the convexity
crossProduct :: Point -> Point -> Point -> Rational
crossProduct (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

isNotConvex :: Shape -> Bool
isNotConvex [] = True
isNotConvex s
  | length s < 3 = error "Checking convexity for a shape with less than 3 points..."
  | otherwise = any (\(a, b, c) -> crossProduct a b c <= 0) pointTriples
  where
    -- basically just rotating the original list to the left by one element
    -- this makes sure we get a list of consectuive edges 
    pointTriples = zip3 s (drop 1 $ cycle s) (drop 2 $ cycle s)

-- Define a function to check if a set is convex and has diameter less than or equal to 1:
isConvexAndDiameter1  :: Shape -> Bool
isConvexAndDiameter1  shape = not (isNotConvex shape) && all (\(p1, p2) -> distance p1 p2 <= 1) pointPairs
  where
    pointPairs = [(p1, p2) | p1 <- shape, p2 <- shape]

sufficientlyLongPrefix :: [Integer] -> [Integer] -> Bool
sufficientlyLongPrefix xs ys = take n xs == take n ys
  where
    n = 100  -- Change this value according to the desired level of approximation

type Transformation = (Rational, Rational)  -- For simplicity, we assume transformations are translations

applyTransformation :: Transformation -> Shape -> Shape
applyTransformation (tx, ty) shape = map (\(x, y) -> (x + tx, y + ty)) shape

isContainedIn :: Shape -> Shape -> Bool
isContainedIn shapeA shapeB = ...  -- Implement this based on your specific containment criterion

-- coveredByCandidate checks if a Shape is covered by a given candidate covering
coveredByCandidate :: Shape -> [Transformation] -> Bool
coveredByCandidate shape transformations = any (\tau -> isContainedIn (applyTransformation tau shape) candidateCovering) transformations

-- Validate whether a given candidate covering is a valid universal covering
validateCovering :: (Shape -> Bool) -> Bool
validateCovering coveredByCandidate = all isValid gridPoints
  where
    isValid point = any (\shape -> isConvexAndDiameter1 shape && sufficientlyLongPrefix (encodeShape shape) (encodePoint point) && coveredByCandidate shape) gridPoints
