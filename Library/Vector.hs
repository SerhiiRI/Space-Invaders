module Library.Vector
  ( Point(..)
  , Dimension(..)
  , parseWindow
  , renderOnPoint
)where


import System.Console.ANSI as ANSI
import qualified System.Console.Terminal.Size as Size


data Point = Point { x :: Int, y :: Int} deriving (Show)
data Dimension = Dimension { height :: Int, width :: Int } deriving (Show)
instance Eq Dimension where
  a@(Dimension ah aw) == b@(Dimension bh bw) = ah == bh && aw == bw
  a@(Dimension ah aw) /= b@(Dimension bh bw) = ah /= bh || aw /= bw

renderOnPoint :: Point -> [String] -> IO ()
renderOnPoint (Point x_position y_position) (x:xs) = do
  ANSI.setCursorPosition y_position x_position
  putStrLn x
  renderOnPoint (Point x_position (y_position+1)) xs
renderOnPoint (Point x_position y_position) [] = do
  ANSI.setCursorPosition y_position x_position


parseWindow :: Maybe (Size.Window Int) -> Dimension
parseWindow (Just (Size.Window {Size.height = h, Size.width = w})) = (Dimension {height=h, width=w})
parseWindow (Nothing) = (Dimension {height=0, width=0})
