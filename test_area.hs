import           AERN2.Real
import qualified MixedTypesNumPrelude as MT
import           Prelude

type Point = (CReal, CReal)
type Shape = [Point]

--asC :: Double -> CReal


pythag :: Point -> Point -> CReal
pythag (x1,y1) (x2,y2) = sqrt ((if abs((x2-x1)) == 0 then 0 else abs((x2-x1))**(2::CReal)) + (if abs((y2-y1)) == 0 then 0 else abs((y2-y1))**(2::CReal)))

distance :: (Point, Point) -> CReal
distance ((x1, y1), (x2, y2)) = if (x1,y1) == (x2,y2) then (0::CReal) else pythag (x1,y1) (x2,y2)
--distance ((x1, y1),(x2,y2)) = if ((x1,y1)==(x2,y2)) then (0.0 ::CReal) else (sqrt(abs((x2 - x1)**(2::CReal))+abs((y2 - y1)**(2::CReal))))


cartesianProd :: Shape -> Shape -> [(Point,Point)]
cartesianProd s1 s2 = [(p,q) | p <- s1, q <- s2]


diameter :: Shape -> CReal
diameter s = if length s == 0 then (0::CReal) else MT.maximum [distance (i) | i <- cartesianProd s s]

a,b,c :: Point
a = (1,2)
b = (2,2)
c = (2.5,3)

--diameter :: Shape -> CReal
--diameter s = if length s == 0 then (0.0 ::CReal) else max [distance(i) | i <- (cartestianProd s s)]

--a,b,c :: Point
--a = ((1.0::CReal),(2.0::CReal))
--b = ((2.0::CReal),(2.0::CReal))
--c = ((2.5:CReal),(3.0::CReal))

tri :: Shape
tri = [a,b,c]
