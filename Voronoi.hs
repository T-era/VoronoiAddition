import System.Environment
import Data.Char
import Graphics.UI.GLUT

import VoronoiGui

data Mode = Mode (IO()) [String] String
instance Show Mode where
	show (Mode _ [] desc) = "\t(no arguments)\n\t\t" ++ desc
	show (Mode _ (o:ls) desc) = "\t" ++ o ++ showOpts ls ++ desc
		where
			showOpts [] = "\n\t\t"
			showOpts (s:ls) = ", " ++ s ++ showOpts ls

allMode = [modeDebug,
	modeGiraffe,
	modeShowUsage,
	modeNormal];

modeDebug = Mode (showWindow debugDraw) ["/d", "-d"] "Show base-point & voronoi-line."
modeGiraffe = Mode (showWindow giraffeFill) ["/g", "-g"] "Show voronoi graph as like giraffe...lovely!"
modeShowUsage = Mode showUsage ["/h", "/?", "-h", "--help"] "Show this usage."
modeNormal = Mode (showWindow normalDraw) [] "Show voronoi-line without base-point."

main = do
	args <- getArgs
	let (Mode action _ _) = selectMode$map (map toLower) args
	action

selectMode :: [String] -> Mode
selectMode [] = modeNormal
selectMode (s:_) = selectModeFirst allMode
        where
                selectModeFirst [] = modeShowUsage
                selectModeFirst (m@(Mode _ options _):ms)
                        | any (s==) options = m
                        | otherwise = selectModeFirst ms

showUsage = do
	(progname, _) <- getArgsAndInitialize
	putStrLn ("Usage: " ++ progname ++ " [Options]")
	putStrLn ("VORONOI GRAPH generator!!")
	putStrLn ("  To add base-points, click someware on window.")
	putStrLn "Options:"
	mapM_ (\m -> putStrLn$show m) allMode
