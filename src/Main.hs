module Main where
import System.IO
main :: IO ()
main = print . f . fmap sum . slide3 . map fread . words =<< readFile "day1.txt"
-- main = print . f . map fread . words =<< readFile "day1.txt"

fread :: String -> Int
fread = read

depths = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] :: [Int]

f :: [Int] -> Int
f [] = 0
f (x:xs) = go (0,x) xs

go :: (Int, Int) -> [Int] -> Int
go (count, _) [] = count
go (count, prev) (curr:rest) = go (if prev < curr then count + 1 else count, curr) rest

-- part two: comparing the sum of 3 sliding windows

splitN :: Int -> [a] -> [[a]]
splitN _ [] = [] 
splitN n xs = take n xs:splitN n (drop n xs)

slide3 :: [Int] -> [[Int]]
slide3 [] = []
slide3 (x:xs) = (x: take 2 xs): slide3 xs