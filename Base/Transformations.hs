module Transformations where

import H_Base
import Data.List (nub, maximumBy, minimumBy, transpose)
import Data.Function (on)
import Data.Ratio ((%))


data Transformation = Translate Point | Rotate Rational Point | Flip Line
-- The Space of Transformations
-- Should be a product space of the Rotation Space, Flipping Space and Translation space.
type SpaceOfTransformations = [Transformation]


-- Rotation Space
-- Take in a shape, a center point about which to rotate, and an angle by which to rotate (theta)
rotate :: Point -> Point -> Rational -> Point
rotate (px, py) (x, y) theta =
  ( x', y' )
  where
    mp = tan (theta / 2)
    x' = (1 - mp^2) / (1 + mp^2) * x + 2 * mp / (1 + mp^2) * y - 2 * px * (1 - mp^2) / (1 + mp^2) - 2 * py * mp / (1 + mp^2)
    y' = 2 * mp / (1 + mp^2) * x + (1 - mp^2) / (1 + mp^2) * y - 2 * px * mp / (1 + mp^2) - 2 * py * (1 - mp^2) / (1 + mp^2)

flip ::
translate ::

-- Relates to Proposition 6 - applying transformation
-- Take in a set, and the transformation, return the transformed set {tau(x) | x \in S}.
applyTransformation :: Transformation -> Shape -> Shape
applyTransformation (Translate (tx, ty)) shape = map (\(x, y) -> (x + tx, y + ty)) shape
applyTransformation (Rotate theta (cx, cy)) shape = map (\(x, y) -> (rCos * (x - cx) - rSin * (y - cy) + cx, rSin * (x - cx) + rCos * (y - cy) + cy)) shape
  where
    rTheta = fromRational theta :: Double
    rCos = toRational (cos rTheta)
    rSin = toRational (sin rTheta)
applyTransformation (Flip (m, (px, py))) shape = map (\(x, y) -> (x', y')) shape
  where
    mp = -1 / m
    x' = (1 - mp^2) / (1 + mp^2) * x + 2 * mp / (1 + mp^2) * y - 2 * px * (1 - mp^2) / (1 + mp^2) - 2 * py * mp / (1 + mp^2)
    y' = 2 * mp / (1 + mp^2) * x + (1 - mp^2) / (1 + mp^2) * y - 2 * px * mp / (1 + mp^2) - 2 * py * (1 - mp^2) / (1 + mp^2)

allTransformations :: SpaceOfTransformations
allTransformations = [Translate (tx, ty) | tx <- [-1..1], ty <- [-1..1]] ++ [Rotate theta (cx, cy) | theta <- [0, pi / 2, pi, 3 * pi / 2],
                                                                             cx <- [0, 1], cy <- [0, 1]] ++ [Flip (m, (px, py)) | m <- [-1, 0, 1], px <- [0, 1],
                                                                              py <- [0, 1]]
