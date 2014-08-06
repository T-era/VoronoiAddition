module VoronoiGL(drawFieldNormal, drawFieldDebug, drawFieldGiraffe) where

import Graphics.Rendering.OpenGL
import qualified Data.Map as Map

import VField
import qualified VPoint as V
import VConvToPoly

type Drawing d = ((V.Point d) -> [VLine d] -> IO())

white = Color4 0.2 0.2 0.2 1
yellow = Color4 1 1 0.5 1

drawFieldNormal :: (Show d, VertexComponent d, Num d) => VField d -> IO()
drawFieldNormal = drawField drawingNormal
drawFieldDebug :: (Show d, VertexComponent d, Fractional d) => VField d -> IO()
drawFieldDebug = drawDebug
drawFieldGiraffe :: (Show d, VertexComponent d, RealFloat d) => VField d -> IO()
drawFieldGiraffe = drawField fillingGiraffe

drawField :: (VertexComponent d, Num d) => Drawing d -> VField d -> IO()
drawField how field = do
	mapM_ (\(bp, vLines) -> how bp vLines) (Map.toList field)
drawDebug :: (Show d, Fractional d, VertexComponent d) => VField d -> IO()
drawDebug field = do
	putStrLn $ show field
	mapM_ (\(bp, vLines) -> drawingDebug bp vLines) (Map.toList field)

drawingNormal :: (Show d, VertexComponent d, Num d) => Drawing d
drawingNormal _ lines = do
	currentColor $= white
	drawLines (elements $ toPoly lines)

drawingDebug :: (Show d, VertexComponent d, Fractional d) => Drawing d
drawingDebug bp lines = do
	currentColor $= white
	drawPoint bp
	drawLines (map (\tpl -> shortenTpl bp tpl) (elements $ toPoly lines))

fillingGiraffe :: (Show d, VertexComponent d, RealFloat d) => Drawing d
fillingGiraffe bp [] = do
	currentColor $= yellow
	fillArc bp 100 (0,2*pi)
fillingGiraffe bp lines = do
	let poly@(Poly el _) = toPoly lines
	currentColor $= yellow
	fillLines bp (map (\tpl -> shortenTpl bp tpl) el)
	drawEdge poly
	where
		drawEdge (Poly _ NoEdges) = return ()
		drawEdge (Poly _ (PolyEdges e1)) = fillLines bp [shortenTpl bp e1]
		drawEdge (Poly _ (DoubleEdges e1 e2)) = fillEdge e1 e2

fillEdge (p1, p2) (p3, p4) = renderPrimitive mode $ do
	toVertex p1
	toVertex p2
	toVertex p3
	toVertex p4
		where
			v1 = fromTo p1 p2
			v2 = fromTo p3 p4
			fromTo (x1, y1) (x2, y2) = V.Vector ((x2 - x1), (y2 - y1))
			mode = if (V.isSameDirection v1 v2)
						then QuadStrip
						else Quads
			

fillArc :: (VertexComponent d, Floating d, Ord d) => (d, d) -> d -> (d, d) -> IO()
fillArc (x, y) r range = renderPrimitive Polygon $ mapM_ vertex (map genArc $ genR range)
	where
		genArc t = Vertex2 (x + r * cos t) (y + r * sin t)
		genR (minR, maxR)
			| minR > maxR = moduloGenerate minR maxR (mod' (2*pi) $)
			| otherwise   = generate minR maxR
		mod' m x
			| x > m = x - m
			| otherwise = x
		generate n x = moduloGenerate n x id
		moduloGenerate minR maxR f
			| minR > pi * 2 = [pi*2]
			| (f minR) > maxR = [f minR]
			| otherwise = (minR:genR (minR+0.01, maxR))

drawLines :: (VertexComponent d) => [(V.Point d, V.Point d)] -> IO ()
drawLines list = renderPrimitive Lines $ mapM_ (\(p1, p2) -> do
	toVertex p1
	toVertex p2) list
fillLines :: (VertexComponent d) => V.Point d -> [(V.Point d, V.Point d)] -> IO ()
fillLines bp list = renderPrimitive Triangles $ mapM_ (\(p1, p2) -> do
	toVertex bp
	toVertex p1
	toVertex p2) list

drawPoint :: (VertexComponent d, Fractional d) => (d, d) -> IO()
drawPoint bp = renderPrimitive Points $ do
	toVertex bp

shortenTpl :: (Fractional d) => V.Point d -> (V.Point d, V.Point d) -> (V.Point d, V.Point d)
shortenTpl bp (p1, p2) = (shortenControl bp p1, shortenControl bp p2)
shortenControl (bx, by) (x, y) = (shorten bx x, shorten by y)
	where
		shorten b a = (a*4 + b) / 5

toVertex (x, y) = vertex $ Vertex2 x y
