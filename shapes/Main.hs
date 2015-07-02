module Main where
import Render
import Transform





cubeTriangles pitch yaw = case (map . map) (translate (0, 0, 16)  . rotate yaw pitch  . translate (-5, -5, -5)) (makePrism (10, 10, 10)) of
   foo -> concat $ zipWith convertShape "abcdef" foo








demo pitch yaw = printRender (0, 0, 0) (0, 0, 0) (cubeTriangles pitch yaw) mySettings



test pitch yaw = doThing' mySettings (0, 0, 0) (0, 0, 0) (cubeTriangles pitch yaw)