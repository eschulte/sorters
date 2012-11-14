import System.Environment
import Data.List

qsort []     = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]

args_to_ints :: [String] -> [Int]
args_to_ints = map read

main :: IO ()
main = getArgs >>= sequence_ . (map putStr) . (\ x -> x++["\n"]) .
       (intersperse " "). (map show) . qsort . args_to_ints
