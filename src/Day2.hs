module Main where
import System.IO
import Text.Read (Lexeme(String))

main :: IO ()
main = print . (\(x,y,z) -> x * y) . move' . tuplify . words =<< readFile "day2.txt"
-- Part 1: answer is 2039256
-- main = print . uncurry (*) . move . tuplify . words =<< readFile "day2.txt"

tuplify :: [String] -> [(Direction,Int)]
tuplify (x1:x2:xs) = encode (x1, x2):tuplify xs
tuplify (_:_) = error "Odd number of elements"
tuplify [] = []

data Direction = Forward | Down | Up
                deriving (Eq, Enum)

toDirection :: String -> Direction
toDirection x = case x of
              "forward" -> Forward
              "up" -> Up
              "down" -> Down

encode :: (String, String) -> (Direction, Int)
encode (x,y) = (toDirection x, read y :: Int)

actions = [("forward", 5),("down", 5), ("forward", 8), 
  ("up", 3), ("down", 8), ("forward", 2)] :: [([Char], Int)]

-- Part 2: added aim value to 3-tuple
move' :: [(Direction, Int)] -> (Int, Int, Int)
move' [] = (0,0,0)
move' xs = foldl f (0,0,0) xs
  where f (a,b,c) (x,y)
          | x == Forward  = (a + y, b + (c * y), c)
          | x == Up       = (a, b, c - y)
          | x == Down     = (a, b, c + y)
          | otherwise       = (a, b, c)

{-- Part 1: only depth
move :: [([Char], Int)] -> (Int, Int)
move [] = (0,0)
move xs = foldl f (0,0) xs
  where f (a,b) (x,y)
          | x == "forward"  = (a + y, b)
          | x == "up"       = (a, b - y)
          | x == "down"     = (a, b + y)
          | otherwise       = (a, b)
--}

