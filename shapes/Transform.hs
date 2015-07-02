module Transform where
import CommonData
import Perspective (s2R, r2S)
import Control.Applicative
import Winding (contains)
import Data.List


g `on` f = \a b -> g (f a) (f b)

toTriangles' corner [_] = []
toTriangles' corner (a:b:rest) = (corner, a, b) : toTriangles' corner (b:rest)


--Assumes a list of points in a circular order, forming a convex polygon
toTriangles (a:rest) = toTriangles' a rest


translate (x, y, z) (a, b, c) = (x + a, y + b, z + c)

rotate yaw pitch p = case r2S p of
   (x, y, z) -> s2R (x + yaw, y + pitch, z)


makePrism res (x, y, z) = [
	dotY res $ makeRect origin (x, 0, 0) (0, 0, z),
	dotY res $ makeRect (0, y, 0) (x, 0, 0) (0, 0, z),
	dotX res $ makeRect origin (0, y, 0) (0, 0, z),
	dotX res $ makeRect (x, 0, 0) (0, y, 0) (0, 0, z),
	dotZ res $ makeRect origin (x, 0, 0) (0, y, 0),
	dotZ res $ makeRect (0, 0, z) (x, 0, 0) (0, y, 0)]



makeRect o a b = [o, translate a o, translate a (translate b o), translate b o]

convertShape :: a -> [Point] -> [(a, Point)]
convertShape f shape = map ((,) f) shape


roll radians = rotate 0 (negate pi / 2) . rotate radians 0 . rotate 0 (pi / 2)


getX (x, _, _) = x
getY (_, y, _) = y


fillWithDots :: Double -> [Point] -> [(Double, Double)]
fillWithDots res shape =
 let
  byX = sortBy (compare `on` getX) shape
  byY = sortBy (compare `on` getY) shape
  minX = getX $ head byX
  maxX = getX $ last byX
  minY = getY $ head byY
  maxY = getY $ last byY
  theDots = (,) <$> [minX, minX + res..maxX] <*> [minY, minY + res..maxY]
  theDotsInside = filter (contains shape) theDots
 in theDotsInside

dotZ res shape@((_, _, z):_) = map (\(a, b) -> (a, b, z)) $ fillWithDots res shape

dotX res shape@((x, _, _):_) = (\(y, z) -> (x, y, z)) <$> fillWithDots res [(y, z, 0) | (_, y, z) <- shape]

dotY res shape@((_, y, _):_) = (\(x, z) -> (x, y, z)) <$> fillWithDots res [(x, z, 0) | (x, _, z) <- shape]


  