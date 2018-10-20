module Library.Dimension
(
  Dimension(..)
)where

data Dimension = Dimension { height :: Int, width :: Int } deriving (Show)
