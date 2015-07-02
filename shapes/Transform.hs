module Transform where
import CommonData
import Perspective (s2R, r2S)
import Control.Applicative



toTriangles' corner [_] = []
toTriangles' corner (a:b:rest) = (corner, a, b) : toTriangles' corner (b:rest)


--Assumes a list of points in a circular order, forming a convex polygon
toTriangles (a:rest) = toTriangles' a rest


translate (x, y, z) (a, b, c) = (x + a, y + b, z + c)

rotate yaw pitch p = case r2S p of
   (x, y, z) -> s2R (x + yaw, y + pitch, z)


makePrism (x, y, z) = [
	makeRect origin (x, 0, 0) (0, 0, z),
	makeRect (0, y, 0) (x, 0, 0) (0, 0, z),
	makeRect origin (0, y, 0) (0, 0, z),
	makeRect (x, 0, 0) (0, y, 0) (0, 0, z),
	makeRect origin (x, 0, 0) (0, y, 0),
	makeRect (0, 0, z) (x, 0, 0) (0, y, 0)]



makeRect o a b = [o, translate a o, translate a (translate b o), translate b o]

convertShape f shape = map ((,) f) $ toTriangles shape


roll radians = rotate 0 (negate pi / 2) . rotate radians 0 . rotate 0 (pi / 2)