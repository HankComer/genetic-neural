module Render where
import Perspective (delta, s2R, r2S, pythag3, pythag2)
import Fill (cFillTriangle, getZ, on)
import Data.List (minimumBy, groupBy, sortBy)
import CommonData

shiftAndRotate :: Point -> Point -> Point -> Point
shiftAndRotate coord view point =  (s2R . flip delta view . r2S . flip delta coord) point



fixTriangle :: RenderSettings -> Point -> Point -> (a, (Point, Point, Point)) -> [(a, Point)]
fixTriangle s coord view (disp, (a, b, c)) = 
 let
  thing = fixPoint s coord view
 in map (fmap (scaleByRetina s . unScale s)) $ cFillTriangle (resolutionOf s) (thing a) (thing b) (thing c) disp

fixPoint :: RenderSettings -> Point -> Point -> Point -> Point
fixPoint s coord view p = case shiftAndRotate coord view p of
   (x, y, z) -> (xScaleOf s * x, yScaleOf s * y, z)

unScale :: RenderSettings -> Point -> Point
unScale s (x, y, z) = (x / xScaleOf s, y / yScaleOf s, z)

doThing' settings coord view = (>>= fixTriangle settings coord view)

doThing s coord view = map (fmap (scaleByRetina s . unScale s . fixPoint s coord view))





renderPoints :: Eq a => RenderSettings -> a -> [(a, Point)] -> [[a]]
renderPoints s empty =
  renderAll (yThreshOf s) (xThreshOf s) empty . map closest . filter (/= []) . map (filter (\(_, (_, a)) -> a > 0)) . byCoords . map byCoord



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
scale s (x, y, z) = (x * xScaleOf s, y * yScaleOf s, z)

-- takes a rectangular point
scaleByRetina :: RenderSettings -> Point -> Point
scaleByRetina s p@(x, y, z) = (pythag2 x z * atan2 x z / atan2 (fromIntegral $ xThreshOf s) (retinaOf s), pythag2 y z * atan2 y z / atan2 (fromIntegral $ yThreshOf s) (retinaOf s), pythag3 p * signum z)
  

scalePoint' (x', y') (a, (x, y, z)) = (a, (x * x', y * y', z))


printRender coord view triangles settings =
  mapM_ putStrLn $ renderPoints settings ' ' $ doThing' settings coord view triangles

displayRender coord view points settings =
  mapM_ putStrLn $ renderPoints settings ' ' $ doThing settings coord view points

scaleTriangle :: RenderSettings -> (a, (Point, Point, Point)) -> (a, (Point, Point, Point))
scaleTriangle settings (r, (a, b, c)) = (r, (scale settings a, scale settings b, scale settings c))




myTriangle f distance = (f, ((-5, 0, distance), (0, 12, distance), (12, 0, distance)))

