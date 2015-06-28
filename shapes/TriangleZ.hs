module TriangleZ where
--Figures out Z given a triangle, and X, and a Y.



type Point = (Double, Double, Double)




delta (x, y, z) (x', y', z') = (x - x', y - y', z - z')

dependentX a b x = case delta a b of
  (x', y, z) -> (x, y * x / x', z * x / x')

dependentY a b y = case delta a b of
  (x, y', z) -> (x * y / y', y, z * y / y')


thing (a:b:c:_) x y = dependentY a (dependentX b c x) y

thing' (a:b:c:_) x y = dependentX a (dependentY b c y) x


blahList :: [Point]
blahList = [(3,3,24), (5,1,23), (3,1,18)]