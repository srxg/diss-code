import Data.List (transpose)
import Data.Ratio ((%))
import Data.List (nub, maximumBy, minimumBy, transpose, subsequences)
import Data.Function (on)
import Data.Ratio ((%))

-- Define the data types for points, shapes, lines and transformations
type Point = (Rational, Rational)
type Shape = [Point]
type Line = (Rational, Point)
data Transformation = Translate Point | Rotate Rational Point | Flip Line
type SpaceOfTransformations = [Transformation]

-- Define a binary tree data type used for the Stern Brocot Tree
data Tree a = Node a (Tree a) (Tree a)

-- Function to calculate the distance between two points
-- (Related to prop 1/2 - diameter of a set)
pythag :: Point -> Point -> Double
pythag (x1,y1) (x2,y2) = sqrt ((abs(fromRational (x2-x1))**2.0) + (abs(fromRational (y2-y1))**2.0))

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = if (x1,y1) == (x2,y2) then 0.0 else pythag (x1,y1) (x2,y2)

-- Function to compute the cartesian product of two shapes
cartesianProd :: Shape -> Shape -> [(Point,Point)]
cartesianProd s1 s2 = [(p,q) | p <- s1, q <- s2]

-- Function to compute the diameter of a shape
-- Relates to Proposition 1 in the dissertation (diameter of a set)
diameter :: Shape -> Rational
diameter s = if null s then 0.0 else maximum [toRational (distance i j) | (i,j) <- cartesianProd s s]

-- Function to compute the cross product of three points
-- It is a helper function for isNotConvex, which is related to Proposition 4 in the disser
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

-- Function to check if a shape is not convex
-- Relates to Proposition 4 in the dissertation (convexity of a set)
isNotConvex :: Shape -> Bool
isNotConvex [] = True
isNotConvex s
  | length s < 3 = error "Checking convexity for a shape with less than 3 points..."
  | otherwise = any (\(a, b, c) -> crossProduct a b c <= 0) pointTriples
  where
    -- basically just rotating the original list to the left by one element
    -- this makes sure we get a list of consectuive edges 
    pointTriples = zip3 s (drop 1 $ cycle s) (drop 2 $ cycle s)

-- Function to convert a rational number to a binary sequence
rationalToBinary :: Rational -> [Integer]
rationalToBinary r = go sternBrocot
  where
    go (Node x left right)
      | r == x = []
      | r < x = 0 : go left
      | otherwise = 1 : go right

-- Function to encode a point as a binary sequence
encodePoint :: Point -> [Integer]
encodePoint (x, y) = interleave (rationalToBinary x) (rationalToBinary y)
  where
    interleave xs ys = concat $ transpose [xs, ys]

-- Function to encode a shape as a binary sequence
encodeShape :: Shape -> [Integer]
encodeShape = concatMap encodePoint

-- Function to create the Stern-Brocot tree
sternBrocot :: Tree Rational
sternBrocot = build 0 1 1 0
  where
    build l1 l2 r1 r2 = Node m (build l1 l2 m1 m2) (build m1 m2 r1 r2)
      where
        m1 = l1 + r1
        m2 = l2 + r2
        m = m1 % m2

-- Function to generate a list of points in the Stern-Brocot tree with x-coordinates between minX and maxX
-- and y-coordinates between 0 and maxY.
generatePoints :: Rational -> Rational -> Rational -> [Point]
generatePoints minX maxX maxY = [(x, y) | x <- sternBrocotList minX maxX, y <- sternBrocotList 0 maxY]

-- Function to generate a list of rational numbers in the Stern-Brocot tree between minX and maxX.
sternBrocotList :: Rational -> Rational -> [Rational]
sternBrocotList minX maxX = filter (\x -> x >= minX && x <= maxX) (toList sternBrocot)
    where
    -- Convert the Stern-Brocot tree to a list of rational numbers.
    toList (Node x left right) = x : (toList left ++ toList right)

-- Function to generate all possible subsequences of a list (i.e., all possible shapes that can be
-- formed by the points in the list) and filters out those that do not satisfy the
-- conditions ¬isNotConvex(S) and diam(D) <= 1.
generateCandidateShapes :: [Point] -> [Shape]
generateCandidateShapes points = filter (\s -> not (isNotConvex s) && diameter s <= 1) (subsequences points)

-- Function to generate the set D of shapes that satisfy the conditions ¬isNotConvex(S) and diam(D) <= 1.
-- See Proposition 4/Section 5.3
generateD :: Rational -> Rational -> Rational -> [Shape]
generateD minX maxX maxY = generateCandidateShapes (generatePoints minX maxX maxY)

-- Functions to apply transformations to shapes
-- *Related* to Apply(S, \tau)
applyTransformation :: Transformation -> Shape -> Shape
applyTransformation (Translate (tx, ty)) shape = [(x + tx, y + ty) | (x, y) <- shape]
applyTransformation (Rotate theta centre) shape = [rotatePoint theta centre point | point <- shape]
applyTransformation (Flip line) shape = [flipPoint line point | point <- shape]

-- Functions to apply a rotation to a point
-- (See the Section on the Transformation Space)
rotatePoint :: Rational -> Point -> Point -> Point
rotatePoint theta (cx, cy) (x, y) = (x', y')
  where
    angle :: Double
    angle = fromRational theta

    x', y' :: Rational
    x' = cx + toRational ((fromRational (x - cx)) * cos angle - (fromRational (y - cy)) * sin angle)
    y' = cy + toRational ((fromRational (x - cx)) * sin angle + (fromRational (y - cy)) * cos angle)


-- Function to apply a flip to a point
-- (See the Section on the Transformation Space)
flipPoint :: Line -> Point -> Point
flipPoint (m, (px, py)) (x, y) = (x', y')
  where
    d = (x + (y - py) * m) / (1 + m * m)
    x' = 2 * d - x
    y' = 2 * d * m - y + 2 * py

-- Function to apply multiple transformations to a shape
-- *Related* to Apply(S, \tau)
applyTransformations :: SpaceOfTransformations -> Shape -> Shape
applyTransformations transformations shape = foldr applyTransformation shape transformations

-- To apply multiple transformations to a shape, you can simply call the applyTransformations function with a list of transformations:
-- transformedShape = applyTransformations [Rotate pi (0, 0), Translate (1, 1), Flip (1, (0, 0))] shape
-- This code snippet would rotate the shape by pi radians around the origin, then translate it by (1, 1), and finally flip it along the line with slope 1 and passing through the origin.