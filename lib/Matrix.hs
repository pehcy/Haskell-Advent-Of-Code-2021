module Matrix (
    Matrix(..),
    getRow,
    getCol
    ) where

newtype Matrix a = Matrix [[a]]
                deriving (Show, Read)

instance Functor Matrix where
    fmap f (Matrix xss) = Matrix (map (map f) xss)

-- get Row, Column Vector
getRow, getCol :: Matrix a -> Int -> [a]
getCol (Matrix mat) n = mat >>= ((:[]) . (!! n))
getRow (Matrix mat) n = mat !! n
