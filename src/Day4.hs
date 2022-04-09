{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Data.Function (on)
import Data.List (groupBy, unfoldr)
import Control.Applicative (Applicative(liftA2))

-- main :: IO [String]
main = do 
  fileLines <- readFile "day4.txt"
  let content = lines fileLines
  let guess = tuplify 5 $ num2Int <$> wordsWhen ((==',')<||>(==' ')) (head content)
  let u = toMatrix $ fmap words <$> sep content
  return guess

num2Int :: String -> Int
num2Int xs = read xs :: Int

sep :: [a] -> [[a]]
sep xs = fmap tail (chunk 6 $ drop 1 xs)
        where chunk n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))

repeatMap :: (Eq t, Num t) => (c -> c) -> t -> c -> c
repeatMap f n = go f n
              where go acc 1 = acc
                    go acc n = go (f . acc) (n - 1)
-- In another way, we can try
-- ** 
-- repeatMap f n = foldr1 (.) $ replicate n f
-- **

(<&&>),(<||>) :: Applicative m => m Bool -> m Bool -> m Bool
(<&&>) = liftA2 (&&)
(<||>) = liftA2 (||)

{---------------------------------------------------------------
To break comma for string, replace p by ((==',')<||>(==' '))
---> wordsWhen ((==',') <||> (==' ')) "foo, bar, snoo" 
---> ["foo", "bar", "snoo"]
----------------------------------------------------------------}
wordsWhen :: (Char -> Bool) -> [Char] -> [[Char]]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s_ -> w : wordsWhen p ss_
                        where (w, ss_) = break p s_


toMatrix :: [[[String]]] -> [Matrix Int]
toMatrix xss = map Matrix ((fmap . fmap . fmap) num2Int xss)

newtype Matrix a = Matrix [[a]]
                deriving (Show, Read)

instance Functor Matrix where 
  fmap f (Matrix xss) = Matrix (map (map f) xss)

-- get Row, Column Vector
getRow, getCol :: Matrix a -> Int -> [a]
getCol (Matrix mat) n = mat >>= ((:[]) . (!! n))
getRow (Matrix mat) n = mat !! n

{---------------------------------------------------------------
fold reduces a list into a single value, 
while unfold generates a list from a given seed value
-}
--chunk :: Int -> [a] -> [[a]]
--chunk n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))
----------------------------------------------------------------}

-- sample inputs --
list :: [Int]
list = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 
        21, 24, 10, 16, 13, 6, 15, 25, 12, 22,
        18, 20, 8, 19, 3, 26, 1]

tuplify :: Int -> [b] -> [[(Int, b)]]
tuplify n xs = groupBy ((==) `on` fst) (zip (concatMap (replicate n) [0..length xs]) xs)

splitLines :: [Char] -> [[Char]]
splitLines [] = []
splitLines cs = let (pre, suf) = break isLineTerminator cs
                in pre : case suf of
                          ('\r' : '\n' : rest)  -> splitLines rest
                          ('\r':rest)           -> splitLines rest
                          ('\n':rest)           -> splitLines rest
                          _                     -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'
