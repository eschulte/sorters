import System.Environment
import Data.List

bsort :: Ord a => [a] -> [a]
bsort s = case _bsort s of
            t | t == s    -> t
              | otherwise -> bsort t
    where _bsort (x:x2:xs) | x > x2    = x2:(_bsort (x:xs))
                           | otherwise = x:(_bsort (x2:xs))
          _bsort s = s

args_to_ints :: [String] -> [Int]
args_to_ints = map read

main :: IO ()
main = getArgs >>= sequence_ . (map putStr) . (\ x -> x++["\n"]) .
       (intersperse " "). (map show) . bsort . args_to_ints
