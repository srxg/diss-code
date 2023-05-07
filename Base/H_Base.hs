module H_Base where

import Data.List (transpose)
import Data.Ratio ((%))


-- Define the types for a Point, Shape, Line and SpaceOfTransformations
type Point = (Rational, Rational)
type Shape = [Point]
type Line = (Rational, Point)
--type SpaceOfTransformations = [Transformation]

data Tree a = Node a (Tree a) (Tree a)

pythag :: Point -> Point -> Double
pythag (x1,y1) (x2,y2) = sqrt ((abs(fromRational (x2-x1))**2.0) + (abs(fromRational (y2-y1))**2.0))

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = if (x1,y1) == (x2,y2) then 0.0 else pythag (x1,y1) (x2,y2)


cartesianProd :: Shape -> Shape -> [(Point,Point)]
cartesianProd s1 s2 = [(p,q) | p <- s1, q <- s2]

-- Diameter - relates to Proposition 1
diameter :: Shape -> Rational
diameter s = if null s then 0.0 else maximum [toRational (distance i j) | (i,j) <- cartesianProd s s]

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

-- Relates to proposition 4
isNotConvex :: Shape -> Bool
isNotConvex [] = True
isNotConvex s
  | length s < 3 = error "Checking convexity for a shape with less than 3 points..."
  | otherwise = any (\(a, b, c) -> crossProduct a b c <= 0) pointTriples
  where
    -- basically just rotating the original list to the left by one element
    -- this makes sure we get a list of consectuive edges 
    pointTriples = zip3 s (drop 1 $ cycle s) (drop 2 $ cycle s)


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


generatePoints :: Rational -> Rational -> Rational -> [Point]
generatePoints minX maxX maxY = [(x, y) | x <- sternBrocotList minX maxX, y <- sternBrocotList 0 maxY]

sternBrocotList :: Rational -> Rational -> [Rational]
sternBrocotList minX maxX = filter (\x -> x >= minX && x <= maxX) (toList sternBrocot)
  where
    toList (Node x left right) = x : (toList left ++ toList right)

generateCandidateShapes :: [Point] -> [Shape]
generateCandidateShapes points = filter (\s -> not (isNotConvex s) && diameter s <= 1) (subsequences points)

generateD :: Rational -> Rational -> Rational -> [Shape]
generateD minX maxX maxY = generateCandidateShapes (generatePoints minX maxX maxY)
