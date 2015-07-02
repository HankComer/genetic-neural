module Perspective where

import CommonData



delta (x, y, z) (x', y', z') =  (x-x', y-y', z-z')



pythag3 (x, y, z) = sqrt (x * x + y * y + z * z)

---- +Z is forward
---- +X is right
---- +Y is down



r2S p@(x, y, z) = (azimuth, altitude, radius) where
  azimuth = atan2 x z
  altitude = asin (y / radius)
  radius = pythag3 p
  
  

pythag2 x y = sqrt $ x * x + y * y


halfPi = pi / 2


s2R (yaw, pitch, dist) = (x, y, z)  where
   y = dist * sin pitch
   z = dist * cos pitch * cos yaw
   x = dist * cos pitch * sin yaw
   





xdist (x, _, z) = sqrt $ x * x + z * z

ydist (_, y, z) = sqrt $ y * y + z * z


origin :: Point
origin = (0, 0, 0)

degrees x = 190 * x / pi

onePerLine :: Show a => [a] -> IO ()
onePerLine = mapM_ print