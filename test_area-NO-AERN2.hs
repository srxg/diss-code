-- This does not use AERN2.

type Point = (Double, Double)
type Shape = [Point]


pythag :: Point -> Point -> Double
pythag (x1,y1) (x2,y2) = sqrt ((abs(x2-x1)**2.0) + (abs(y2-y1)**2.0))

distance :: (Point, Point) -> Double
distance ((x1, y1), (x2, y2)) = if (x1,y1) == (x2,y2) then 0.0 else pythag (x1,y1) (x2,y2)


cartesianProd :: Shape -> Shape -> [(Point,Point)]
cartesianProd s1 s2 = [(p,q) | p <- s1, q <- s2]

diameter :: Shape -> Double
diameter s = if null s then 0.0 else maximum [distance i | i <- cartesianProd s s]


-- this is made as a helper for the isNotConvex function below
-- the idea here is to use the z-component of the cross product
-- of the 3d versions of the 2d vectors to determine the convexity
crossProduct :: Point -> Point -> Point -> Double
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

a,b,c :: Point
a = (1,2)
b = (2,2)
c = (2.5,3)

tri :: Shape
tri = [a,b,c]
