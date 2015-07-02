module Main where
import Render
import Transform
import CommonData





cubeTriangles pitch yaw = case (map . map) (translate (0, 0, 63)  . rotate yaw pitch  . translate (-5, -5, -5)) (makePrism (resolutionOf mySettings) (10, 10, 10)) of
   foo -> concat $ zipWith convertShape "12+-ab" foo








demo pitch yaw = displayRender (0, 0, 0) (0, 0, 0) (cubeTriangles pitch yaw) mySettings

degRad x = pi * x / 180

demoD a b = demo (degRad a) (degRad b)



test pitch yaw = doThing mySettings (0, 0, 0) (0, 0, 0) (cubeTriangles pitch yaw)


stepThrough a b = mapM_ (\(x, y) -> demoD x y >> getLine) (zip (cycle a) (cycle b))