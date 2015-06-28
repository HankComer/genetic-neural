module Perspective where





type Point = (Double, Double, Double)



delta (x, y, z) (x', y', z') =  (x-x', y-y', z-z')



pythag (x, y, z) = sqrt (x * x + y * y + z * z)

---- +Z is forward
---- +X is right
---- +Y is down



polarRectangular p@(x, y, z) = (atan2 x z, atan2 y z, pythag p)


shiftPerspective :: Point -> Point -> [Point] -> [Point]
shiftPerspective vcoords vprops points = map (flip delta vprops . polarRectangular . flip delta vcoords) points


origin :: Point
origin = (0, 0, 0)

degrees x = 190 * x / pi

onePerLine :: Show a => [a] -> IO ()
onePerLine = mapM_ print