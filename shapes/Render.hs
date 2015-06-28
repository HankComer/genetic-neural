module Render where
import Perspective (delta, s2R, r2S)
import Fill (cFillTriangle, getZ, on)
import Data.List (minimumBy, groupBy, sortBy)

type Point = (Double, Double, Double)

shiftAndRotate :: Point -> Point -> [Point] -> [Point]
shiftAndRotate coord view points = map (s2R . flip delta view . r2S . flip delta coord) points

fixTriangle settings coord view (disp, (a, b, c)) = case shiftAndRotate coord view [a, b, c] of
    [q, w, e] -> map (scalePoint' (xScaleIs settings, yScaleIs settings)) $ cFillTriangle q w e disp




doThing' settings coord view = (>>= fixTriangle settings coord view) . map (scaleTriangle settings)



data RenderSettings = Settings {
	yScaleIs :: Double,
	xScaleIs :: Double,
	yThreshIs :: Int,
	xThreshIs :: Int,
	yawScaleIs :: Double,
	pitchScaleIs :: Double}



renderPoints :: Eq a => RenderSettings -> a -> [(a, Point)] -> [[a]]
renderPoints (Settings yScale xScale yThresh xThresh _ _) empty =
  renderAll yThresh xThresh empty . map closest . filter (/= []) . map (filter (\(_, (_, a)) -> a > 0)) . byCoords . map byCoord



byCoord (c, (x, y, z)) = ((truncate x, truncate y), (c, z))
byCoords = groupBy ((==) `on` fst) . sortBy (compare `on` fst)
closest :: Eq a => [((Int, Int), (a, Double))] -> ((Int, Int), (a, Double))
closest = minimumBy (compare `on` (snd . snd))
renderPoint empty coords x y = case lookup (x, y) coords of
  Just (c, _) -> c
  Nothing -> empty
renderRow xThresh empty coords y = map (\x -> renderPoint empty coords x y) [(-xThresh)..xThresh]
renderAll yThresh xThresh empty coords = map (\y -> renderRow xThresh empty coords y) $ reverse [(-yThresh)..yThresh]

scale :: RenderSettings -> Point -> Point
scale (Settings yScale xScale _ _ _ _) (x, y, z) = (x * xScale, y * yScale, z)

scaleSpherical settings = (\(a, (x, y, z)) -> (a, (x * yawScaleIs settings, y * pitchScaleIs settings, z)))

scalePoint' (x', y') (a, (x, y, z)) = (a, (x * x', y * y', z))


printRender coord view triangles settings =
  mapM_ putStrLn $ renderPoints settings ' ' $ map (scaleSpherical settings) $ doThing' settings coord view triangles

scaleTriangle :: RenderSettings -> (a, (Point, Point, Point)) -> (a, (Point, Point, Point))
scaleTriangle settings (r, (a, b, c)) = (r, (scale settings a, scale settings b, scale settings c))


mySettings = Settings 8 25 12 40 1 1

myTriangle f distance = (f, ((-1, -1, distance), (0, 12, distance), (12, 0, distance)))