module Fill where
import Control.Applicative
import Data.List


--Assuming pixel size is 1

type Point = (Double, Double, Double)

f `on` a = \x y -> f (a x) (a y)


xzBasedOnY start@(x1, y1, z1) end@(x2, y2, z2) y = (x1 + y * deltaX / deltaY, y1 + y, z1 + y * deltaZ / deltaY) where
  deltaY = y2 - y1
  deltaX = x2 - x1
  deltaZ = z2 - z1
  

yzBasedOnX start@(x1, y1, z1) end@(x2, y2, z2) x = (x1 + x, y1 + x * deltaY / deltaX, z1 + x * deltaZ / deltaX) where
  deltaY = y2 - y1
  deltaX = x2 - x1
  deltaZ = z2 - z1


getX (x, _, _) = x
getY (_, y, _) = y
getZ (_, _, z) = z

rangeOf a b = [0..(b - a)]



-- assumes that a is highest, and c is to the right of b
fillTriangle :: Point -> Point -> Point -> a -> [(a, Point)]
fillTriangle a@(_, aY, _) b@(bX, _, _) c@(cX, _, _) color =
 let
  lowerPoints = map (yzBasedOnX b c) (rangeOf bX cX)
  blah p@(_, pY, _) = map (xzBasedOnY p a) (rangeOf pY aY)
 in fmap ((,) color) $ lowerPoints >>= blah


  

cFillTriangle a b c color = case rearrange a b c of
    (top, left, right) -> fillTriangle top left right color

rearrange a b c = 
 let
  [foo, bar, top] = sortBy (compare `on` getY) [a, b, c]
  (left:right:[]) = sortBy (compare `on` getX) [foo, bar]
 in (top, left, right)

onePerLine :: Show a => [a] -> IO ()
onePerLine = mapM_ print