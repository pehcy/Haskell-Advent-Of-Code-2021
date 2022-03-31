{-# LANGUAGE FlexibleInstances #-}
module Main where
import Data.List (group)
import Data.Char (digitToInt)
import Data.Bits (Bits(xor))
import Text.Read (Lexeme(String))

compare2 :: Ord a => [a] -> Char
compare2 [x, y] = if x > y then '0' else '1'
compare2 [] = '\00'
compare2 _ = '\00'

main :: IO ()
main = print . lifeRate . words =<< readFile "day3.txt"

-- sample input byteString --
list :: [[Char]]
list = ["00100", "11110", "10110", "10111", "10101", "01111",
        "00111", "11100", "10000", "11001", "00010", "01010"]

reapply :: Int -> (b -> b) -> b -> b
reapply n f = foldr (.) id (replicate n f)

lifeRate :: [[Char]] -> Int
lifeRate xs = product (map (convert2Dec . fmap digitToInt . concat) ([getO2, getCO2] <*> [xs]))

getO2 :: [[Char]] -> [[Char]]
getO2 xs = go xs 0
	where
		f n xs' = filter ((== groupNth n xs') . (!! n)) xs'
		go xs' n
		    | length xs' /= 1 	= go (f n xs') (n+1)
		    | otherwise 	= xs'

getCO2 :: [[Char]] -> [[Char]]
getCO2 xs = go xs 0
	where
		f n xs' = filter ((== flip' (groupNth n xs')) . (!! n)) xs'
		flip' c	= head . show $ xor (digitToInt c) 1
		go xs' n
		    | length xs' /= 1	= go (f n xs') (n+1)
		    | otherwise 	= xs'

epsilon :: [Char] -> [Char]
epsilon [] = ""
epsilon a = let 
		repl '0' = '1'
 		repl '1' = '0'
		repl _ = '\00'
 	    in map repl a

convert2Dec :: [Int] -> Int
convert2Dec [] = 0
convert2Dec (x:xs) = x * (2 ^ length xs) + convert2Dec xs

countChar :: Char -> [Char] -> Int
countChar c str = length $ filter (== c) str

groupNth :: Int -> [[Char]] -> Char
groupNth _ [] = '\00'
groupNth n xs = compare2 ([countChar '0', countChar '1'] <*> [str])
                where str = (!! n) <$> xs

gamma :: [[Char]] -> [Char]
gamma bStrList = [groupNth c bStrList | c <- [0..length $ drop 1 $ head bStrList]]

powerUsage :: [[Char]] -> Int
powerUsage xs = product (map (convert2Dec . fmap digitToInt) ([gamma, epsilon . gamma] <*> [xs]))
