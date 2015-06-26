module Network where
import Control.Applicative
import System.Random

doubleStream :: [Double]
doubleStream = randoms $ mkStdGen 5

eulers :: Double
eulers = 2.718281828459045

output :: [Double] -> [Double] -> Double
output a = sigmoid . sum . zipWith (*) a

sigmoid :: Double -> Double
sigmoid = recip . succ . exp . negate


type Neuron = [Double]

type Layer = [Neuron]

type Network = [Layer]

doLayer :: Layer -> [Double] -> [Double]
doLayer neurons inputs = map (output inputs) neurons


--Starts at the left
doNetwork :: Network -> [Double] -> [Double]
doNetwork layers inputs = foldr doLayer inputs (reverse layers)



chunks :: Int -> [a] -> [[a]]
chunks n a = take n a : chunks n (drop n a)


makeNeuron :: Int -> [Double] -> (Neuron, [Double])
makeNeuron x ds = (take x ds, drop x ds)

makeLayer :: (Int, Int) -> [Double] -> (Layer, [Double])
makeLayer (inputs, 0) seed = ([], seed)
makeLayer (inputs, number) seed = case makeNeuron inputs seed of
   (neuron, seed') -> case makeLayer (inputs, number - 1) seed' of
      (layer, seed'') -> (neuron:layer, seed'')

makeNetwork' :: [(Int, Int)] -> [Double] -> (Network, [Double])
makeNetwork' [] seed = ([], seed)
makeNetwork' (blah:rest) seed = case makeLayer blah seed of
   (layer, seed') -> case makeNetwork' rest seed' of
      (network, seed'') -> (layer:network, seed'')

makeNetwork :: Int -> [Int] -> [Double] -> Network
makeNetwork inputs sizes seed = fst $ makeNetwork' (specs inputs sizes) seed


specs :: Int -> [Int] -> [(Int, Int)]
specs inputs sizes = zip (inputs:sizes) sizes

networkSize :: [(Int, Int)] -> Int
networkSize = sum . map (uncurry (*))


unMake :: Network -> [Double]
unMake = concat . concat 


isInverse :: Eq a => (a -> b) -> (b -> a) -> a -> Bool
isInverse f g a = a == (g . f $ a)

makeXor = makeNetwork 2 [2, 1]

