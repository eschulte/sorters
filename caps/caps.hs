module Main (main)
where
import qualified Data.ByteString.Char8 as B
import Data.Char (chr,ord)

isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\t' = True
isSpace '\n' = True
isSpace '\r' = True
isSpace _    = False

toUpper :: Char -> Char
toUpper x
  | (x >= 'a' && x <= 'z') = chr $ ord x - 32
  | otherwise = x

fun :: Char -> Char -> Char
fun a b | isSpace a = toUpper b
        | otherwise = b

convert :: B.ByteString -> B.ByteString
convert = B.tail . B.scanl fun ' '

main = B.readFile "large" >>= B.putStr . convert
