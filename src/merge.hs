import System.Environment
import Data.List

merge []         ys                     = ys
merge xs         []                     = xs
merge xs@(x:xs') ys@(y:ys') | x <= y    = x : merge xs' ys
                            | otherwise = y : merge xs  ys'
 
split (x:y:zs) = let (xs,ys) = split zs in (x:xs,y:ys)
split [x]      = ([x],[])
split []       = ([],[])
 
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = let (as,bs) = split xs
                in merge (mergeSort as) (mergeSort bs)

args_to_ints :: [String] -> [Int]
args_to_ints = map read

main :: IO ()
main = getArgs >>= sequence_ . (map putStr) . (\ x -> x++["\n"]) .
       (intersperse " "). (map show) . mergeSort . args_to_ints
