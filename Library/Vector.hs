module Library.Vector
  ( Point(..)
  , Dimension(..)
  , parseWindow
  , isCompatibleWindow
)where
import System.Console.ANSI as ANSI
import qualified System.Console.Terminal.Size as Size

data Point      = Point { x :: Int, y :: Int}                   deriving (Show)
data Dimension  = Dimension { height :: Int, width :: Int }     deriving (Show)
instance Eq Dimension where
  a@(Dimension ah aw) == b@(Dimension bh bw) = ah == bh && aw == bw
  a@(Dimension ah aw) /= b@(Dimension bh bw) = ah /= bh || aw /= bw

parseWindow :: Maybe (Size.Window Int) -> Dimension
parseWindow (Nothing) = (Dimension {height=0, width=0})
parseWindow (Just (Size.Window {Size.height = h, Size.width = w}))
  | and [w > 80, h > 45]                = (Dimension {height=h, width=w})
  | otherwise                           = (Dimension {height=0, width=0})

isCompatibleWindow :: Dimension -> Bool
isCompatibleWindow (Dimension h w) = not $ h==w && h==0
