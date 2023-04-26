--import GHC.Types (Multiplicity(One))
-- This does not use AERN2.
import Control.Monad
import Data.List (nub, maximumBy, minimumBy)
import Data.Function (on)
import Data.Ratio ((%))


type Point = (Rational, Rational)
type Shape = [Point]

leftMost :: [Point] -> Point
leftMost = minimumBy (compare `on` fst)

rightMost :: [Point] -> Point
rightMost = maximumBy (compare `on` fst)


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
-- imagine it being crossproduct A B C
-- Think about the consecutive vectors AB and BC.
-- Put them into a 3d space and set the z-cords to 0.
-- A (x1,y1,0) 
-- B (x2,y2,0)
-- C (x3,y3,0)
-- AB = (x2-x1,y2-y1,0) write as (ux,uy,0)
-- BC = (x3-x2,y3-y2,0) write as (vx,vy,0)
-- Get the cross product AB X BC = (0,0,(ux * vy)- (vx * uy)) (eval it out and you'll see it matches)
-- why? the result's z component (ux*vy)-(vx*uy)
-- gives the direction of the angle formed by AB and BC.
-- If it's positive, the angle is counterclockwise (?) and thus a convex angle
-- if it's negative, the angle is clockwise and thus not a convex angle.

isNotConvex :: Shape -> Bool
isNotConvex [] = True
isNotConvex s
  | length s < 3 = error "Checking convexity for a shape with less than 3 points..."
  | otherwise = any (\(a, b, c) -> crossProduct a b c <= 0) pointTriples
  where
    -- basically just rotating the original list to the left by one element
    -- this makes sure we get a list of consectuive edges 
    pointTriples = zip3 s (drop 1 $ cycle s) (drop 2 $ cycle s)

-- Generate all possible points in the grid within [0,1] x [0,1]
-- This code defines rationalList as a list starting with 0 and 1/2, followed by the sequence of rationals 1/n for n in [3, 4, 5, ...].
-- The double-dot notation (..) is not used here, as it is for specifying ranges of integers. Instead, we use a list comprehension to
-- generate the sequence of rationals.
gridPoints :: [Point]
gridPoints = [(x, y) | x <- rationalList, y <- rationalList]
  where
    rationalList = [0, 1 % 2] ++ [1 % n | n <- [3..]]

-- Define a function to get subsets of a list:
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- Define a function to check if a set is convex and has diameter less than or equal to 1:
isConvexDiameter1 :: Shape -> Bool
isConvexDiameter1 shape = not (isNotConvex shape) && all (\(p1, p2) -> distance p1 p2 <= 1) pointPairs
  where
    pointPairs = [(p1, p2) | p1 <- shape, p2 <- shape]

-- Generate all convex sets of diameter 1:
convexSetsOfDiameter1 :: [Shape]
convexSetsOfDiameter1 = filter isConvexDiameter1 (subsets gridPoints)

partitionPoints :: Point -> Point -> [Point] -> [Point]
partitionPoints a b points = filter (\p -> distance a b p > 0) points

quickHull' :: Point -> Point -> [Point] -> [Point]
quickHull' a b points
  | null points = []
  | otherwise = p : (quickHull' a p leftPoints) ++ (quickHull' p b rightPoints)
  where
    p = maximumBy (compare `on` distance a b) points
    leftPoints = partitionPoints a p points
    rightPoints = partitionPoints p b points

quickHull :: [Point] -> [Point]
quickHull points = hull
  where
    a = leftMost points
    b = rightMost points
    hull = a : b : (quickHull' a b points) ++ (quickHull' b a points)


-- This implementation assumes that both the coveringShape and set are convex shapes.
covers :: Shape -> Shape -> Bool
covers coveringShape set = convexHullCovering == convexHullUnion
  where
    convexHullCovering = quickHull coveringShape
    convexHullUnion = quickHull (nub $ coveringShape ++ set)
    

coversAll :: Shape -> Bool
coversAll coveringShape = all (covers coveringShape) convexSetsOfDiameter1

