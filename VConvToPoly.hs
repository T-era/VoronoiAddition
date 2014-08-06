module VConvToPoly(Poly(Poly, elements, edges), PolyEdges(NoEdges, PolyEdges, DoubleEdges), toPoly) where

import VField
import VLine(Line(Line, CLine, SLine))
import VPoint(Point, Vector(Vector))

data Poly d = Poly { elements::[(Point d, Point d)], edges::PolyEdges d } deriving Show
data PolyEdges d = PolyEdges (Point d, Point d)
	| MidWay (Point d)
	| NoEdges
	| DoubleEdges (Point d, Point d) (Point d, Point d) deriving Show

toPoly :: (Show d, Num d) => [VLine d] -> Poly d
toPoly list = toPolyList list (Poly [] NoEdges)
toPolyList [] x = x
toPolyList ((_, l):ls) poly = toPolyList ls (toPolySingle l poly)
toPolySingle (Line p d) poly@(Poly ol NoEdges) = poly { elements = (newTpl:ol), edges = PolyEdges newTpl }
	where newTpl = ((p-|<d),(p-|>d))
toPolySingle (Line p d) poly@(Poly ol (PolyEdges edge)) = poly { elements = (newTpl:ol), edges = DoubleEdges edge newTpl }
	where newTpl = ((p-|<d),(p-|>d))
toPolySingle (CLine p1 p2) poly@(Poly ol _) = poly { elements = ((p1, p2):ol) }
toPolySingle (SLine p v) poly@(Poly ol NoEdges) = poly { elements = ((p, p-|>v):ol), edges = MidWay (p-|>v) }
toPolySingle (SLine p v) poly@(Poly ol (MidWay edge1)) = poly { elements = ((p, p-|>v):ol), edges = PolyEdges (edge1, (p-|>v)) }
toPolySingle l p = error (show l ++ show p)

(x, y) -|> (Vector (dx, dy)) = (x + 5*dx, y + 5*dy)
(x, y) -|< (Vector (dx, dy)) = (x - 5*dx, y - 5*dy)