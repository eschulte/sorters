import System.Environment
import Data.List

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

args_to_ints :: [String] -> [Int]
args_to_ints = map read

main :: IO ()
main = getArgs >>= sequence_ . (map putStr) . (\ x -> x++["\n"]) .
       (intersperse " "). (map show) . insertionSort . args_to_ints
