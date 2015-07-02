module Fill where
import Control.Applicative
import Data.List
import CommonData

f `on` a = \x y -> f (a x) (a y)


xzBasedOnY start@(x1, y1, z1) end@(x2, y2, z2) y = (x1 + y * deltaX #/ deltaY, y1 + y, z1 + y * deltaZ #/ deltaY) where
  deltaY = y2 - y1
  deltaX = x2 - x1
  deltaZ = z2 - z1
  

yzBasedOnX start@(x1, y1, z1) end@(x2, y2, z2) x = (x1 + x, y1 + x * deltaY #/ deltaX, z1 + x * deltaZ #/ deltaX) where
  deltaY = y2 - y1
  deltaX = x2 - x1
  deltaZ = z2 - z1


infixl 7 #/
(#/) :: Double -> Double -> Double
0 #/ 0 = 0
a #/ 0 = error "divided by zero, check Fill.hs"
a #/ b = a / b


getX (x, _, _) = x
getY (_, y, _) = y
getZ (_, _, z) = z

setX f (x, y, z) = (f x, y, z)
setY f (x, y, z) = (x, f y, z)
setZ f (x, y, z) = (x, y, f z)

rangeOf step a b = if abs (b - a) >= step then rangeOf' step a b else [0]

rangeOf' step a b
 | a < b = [0, step..(b - a)]
 | a > b = map negate [0, step..(a - b)]



-- assumes that a is highest, and c is to the right of b
fillTriangle :: Double -> Point -> Point -> Point -> a -> [(a, Point)]
fillTriangle resolution a@(_, aY, _) b@(bX, _, _) c@(cX, _, _) color =
 let
  lowerPoints = map (yzBasedOnX b c) (rangeOf resolution bX cX)
  blah p@(_, pY, _) = map (xzBasedOnY p a) (rangeOf resolution pY aY)
 in fmap ((,) color) $ lowerPoints >>= blah


  

cFillTriangle resolution a b c color = case rearrange a b c of
    (top, left, right) -> fillTriangle resolution top left right color

rearrange a b c = 
 let
  [foo, bar, top] = sortBy (compare `on` getY) [a, b, c]
  (left:right:[]) = sortBy (compare `on` getX) [foo, bar]
  (qwer, asdf, zxcv)
   | (getX left /= getX right) = (top, left, right) 
   | otherwise = (top, setX (\x -> x - (abs x / 1000)) bar, foo) 
 in (qwer, asdf, zxcv)

