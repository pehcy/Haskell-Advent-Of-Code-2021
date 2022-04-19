{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Day2 where
import Matrix ( Matrix(..), Coord(..) )
import Data.Function (on)
import Data.List (groupBy, unfoldr, group, elemIndex)
import Control.Applicative (Applicative(liftA2))
import Control.Monad (unless)

-- stop until certail condition, same as "until" in Prelude
converge :: (a -> Maybe a) -> a -> a
converge f x  = maybe x (converge f) (f x)

(.:^) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
(.:^) = fmap . fmap

-- getBingo input board2d = until (\x -> checkMatches input x) (checkMatches input board2d)

merge :: [a] -> [a] -> [a]
merge ls [] = ls
merge [] rs = rs
merge (x:xs) (y:ys) = x : y : merge xs ys

-- getBingo :: (Foldable t, Eq a, Functor g) => t a -> g [[a]] -> g [(Int, Int)]
checkBingo guess boards = let
        matches = checkMatches guess <$> boards
        xlist = f' (x .:^ matches)
        ylist = f' (y .:^ matches)
        in xlist `merge` ylist
        where   f' xs = filter (any ((== 5) . snd))
                        ((\x -> (head x, length x))
                        .:^ (group <$> xs))

-- Iterate loop until a condition is met
takeUntil :: Foldable t => (a -> Bool) -> t a -> [a]
takeUntil p = foldr (\x r -> if not (p x) then x:r else [x]) []

-- main :: IO [String]
main = do 
  fileLines <- readFile "day4.txt"
  let content = lines fileLines
  let guess = num2Int <$> wordsWhen ((==',')<||>(==' ')) (head content)
  let boards = toMatrix $ fmap words <$> sep content
  let g = takeUntil (\x -> not (checkBingo (take x guess) boards `setEq` [])) [0..]
  -- let list = head 
  --     $ dropWhile (setEq []) 
  --      $ flip checkBingo boards 
  --      . flip take guess <$> [1..]
  return g

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

checkMatches drawn tiles = [
  Coord x y
 | (y, row) <- enumerate tiles
 , (x, tile) <- enumerate row
 , tile `elem` drawn]

num2Int :: String -> Int
num2Int xs = read xs :: Int

sep :: [a] -> [[a]]
sep xs = fmap tail (chunk 6 $ drop 1 xs)
        where chunk n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))

setEq :: (Foldable t1, Foldable t2, Eq a) => t2 a -> t1 a -> Bool
setEq a1 a2 = all (`elem` a1) a2 && all (`elem` a2) a1

repeatMap :: (Eq t, Num t) => (c -> c) -> t -> c -> c
repeatMap f n = go f n
              where go acc 1 = acc
                    go acc n = go (f . acc) (n - 1)
-- In another way, we can try
-- >> repeatMap f n = foldr1 (.) $ replicate n f

(<&&>),(<||>) :: Applicative m => m Bool -> m Bool -> m Bool
(<&&>) = liftA2 (&&)
(<||>) = liftA2 (||)

{---------------------------------------------------------------
To break comma for string, replace p by ((==',')<||>(==' '))
>> wordsWhen ((==',') <||> (==' ')) "foo, bar, snoo" 
 ["foo", "bar", "snoo"]
----------------------------------------------------------------}
wordsWhen :: (Char -> Bool) -> [Char] -> [[Char]]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s_ -> w : wordsWhen p ss_
                        where (w, ss_) = break p s_

toMatrix :: [[[String]]] -> [[[Int]]]
toMatrix = (fmap . fmap . fmap) num2Int
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
