module Winding where
--Using the c++ code found at "http://geomalgorithms.com/a03-_inclusion.html"





data Foo = Point {x :: Double, y :: Double}


isLeft p0 p1 p2 = (x p1 - x p0) * (y p2 - y p0) - (x p2 - x p0) * (y p1 - y p0)


crossings' cn i p v n
 | i >= n = cn `mod` 2 == 1
 | not $ ((y (v !! i) <= y p) && (y (v !! succ i) > y p))
      || ((y (v !! i) > y p) && (y (v !! succ i) <= y p)) = crossings' cn (succ i) p v n
 | not $ x p < x (v !! i) + ((y p - y (v !! i)) / (y (v !! succ i) - y (v !! i))) * (x (v !! succ i) - x (v !! i)) = crossings' cn (succ i) p v n
 | otherwise = crossings' (succ cn) (succ i) p v n

inside' p v@(a:_) = crossings' 0 0 p (v ++ [a]) (length v)


shape `contains` (x, y) =  inside' (Point x y) [Point a b | (a, b, _) <- shape]