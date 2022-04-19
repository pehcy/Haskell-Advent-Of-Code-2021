{-# LANGUAGE MultiParamTypeClasses #-}

module Matrix (
    Matrix(..),
    Coord(..),
    Bingo(..),
    getFinalScore,
    getRow,
    getCol
    ) where

newtype Matrix a = Matrix [[a]]
                deriving (Show, Read)

instance Functor Matrix where
    fmap f (Matrix xss) = Matrix (map (map f) xss)

data Coord = Coord { x :: Int, y:: Int } deriving (Eq, Show)

data Bingo = Bingo {
                nthSet :: Int,
                lastCalled :: Int,
                marked :: [Int]
             } deriving (Read, Show)


getFinalScore (Bingo s_ c_ m_) guess mat = let 
    sumOfUnmarked = sum [x | x <- concat (mat !! s_), x `notElem` m_]
    in sumOfUnmarked * c_

-- get Row, Column Vector
getRow, getCol :: Matrix a -> Int -> [a]
getCol (Matrix mat) n = mat >>= ((:[]) . (!! n))
getRow (Matrix mat) n = mat !! n
