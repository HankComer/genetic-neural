module Genetic where
import System.Random
import Network

roll :: Int -> IO Bool
roll percent = fmap (< percent) (randomRIO (0, 99))

cross :: ([a], [a]) -> IO ([a], [a])
cross (a, b) = fmap unzip $ mapM (flipRoll 50) (zip a b)

flipRoll :: Int -> (a, a) -> IO (a, a)
flipRoll chance (a, b) = do
    x <- roll chance
    return $ if x then (b, a) else (a, b)

mutate :: Random a => Int -> [a] -> IO [a]
mutate chance = mapM mut where
   mut x = do
    a <- roll chance
    if a then randomIO else return x




--lower is better
calculateFitness :: [Double] -> [Double] -> Double
calculateFitness a b =
 let
  foo = zipWith (-) a b
  bar = zipWith (*) foo foo
 in sum bar