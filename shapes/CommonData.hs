module CommonData where

type Point = (Double, Double, Double)

origin :: Point
origin = (0, 0, 0)



data RenderSettings = Settings {
	yScaleOf :: Double,
	xScaleOf :: Double,
	yThreshOf :: Int,
	xThreshOf :: Int,
	yawScaleOf :: Double,
	pitchScaleOf :: Double,
        resolutionOf :: Double,
        retinaOf :: Double}
mySettings = Settings
	{yScaleOf = 8
	,xScaleOf = 8
	,yThreshOf = 10
	,xThreshOf = 10
	,retinaOf = 10
	,yawScaleOf = 1
	,pitchScaleOf = 1
	,resolutionOf = 0.5
	}