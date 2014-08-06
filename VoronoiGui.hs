module VoronoiGui(showWindow, normalDraw, debugDraw, giraffeFill) where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import VoronoiGL
import VField

initSize = Size 200 200

data DrawMode = DrawMode (Color4 GLfloat) (VField GLfloat -> IO())

normalDraw = DrawMode black drawFieldNormal
debugDraw = DrawMode black drawFieldDebug
giraffeFill = DrawMode yellow drawFieldGiraffe
black = Color4 0 0 0 1
yellow = Color4 0.625 0.5 0 1

startField = emptyField
{-startField = 
	addPoint (f (71,66)) $
	addPoint (f (69,65)) $
	addPoint (f (71,64)) $
	addPoint (f (70,66)) $
	addPoint (f (68,67)) $
	addPoint (f (69,68)) $
	addPoint (f (70,64)) $
	emptyField
f (x, y) = (f' x, f' y)
	where f' = id-}

showWindow (DrawMode backColor howDraw) = do
	field <- newIORef (startField::VField GLfloat)
	windowSize <- newIORef initSize
	initialWindowSize $= initSize
	createWindow "Voronoi?"
	clearColor $= backColor
	let repaint = showField howDraw
	keyboardMouseCallback $= Just (myKeyMouseEvent field repaint)
	reshapeCallback $= Just (myResizeEvent windowSize field repaint)
	displayCallback $= repaint field

	reScale (Size 2 (-2)) initSize
	translateBack initSize
	mainLoop

myKeyMouseEvent :: IORef (VField GLfloat) -> (IORef (VField GLfloat) -> IO()) -> Key -> KeyState -> Modifiers -> Position -> IO()
myKeyMouseEvent field repaint (MouseButton LeftButton) Up _ p = do
	let lp = toPoint p
	putStrLn $ show lp
	modifyIORef field (addPoint lp$)
	repaint field
myKeyMouseEvent _ _ _ _ _ _ = return()

toPoint (Position x y) = (rx, ry)
	where
		rx = (realToFrac x)
		ry = (realToFrac y)

myResizeEvent :: IORef Size -> IORef (VField GLfloat) -> (IORef (VField GLfloat) -> IO()) -> Size -> IO()
myResizeEvent windowSize field repaint size = do
	viewport $= (Position 0 0, size)
	let (Size w h) = size
	when (w > 0 && h > 0) $ do
		origin <- readIORef windowSize
		modifyIORef windowSize (\_ -> size)
		reScale origin size
		repaint field

showField howDraw field = do
	clear [ColorBuffer]
	f <- readIORef field
	howDraw f
	flush

reScale (Size ow oh) (Size w h) = scale ((realToFrac ow/realToFrac w)::GLfloat) (realToFrac oh/realToFrac h)	1
translateBack (Size w h) = translate $ Vector3 x y 1
	where
		x :: GLfloat
		x = (realToFrac w) / (-2)
		y = (realToFrac h) / (-2)
