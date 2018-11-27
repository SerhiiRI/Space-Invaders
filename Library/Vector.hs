module Library.Vector
  ( Point(..)
  , Dimension(..)
  , X
  , Y
  , parseWindow
  , clearScreenToBegin
  , textIntro
  , textFinish
  , isCompatibleWindow
)where
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Terminal.Size as Size

type X = Int
type Y = Int

data Point      = Point { x :: Int, y :: Int}                   deriving (Show)
data Dimension  = Dimension { height :: Int, width :: Int }     deriving (Show)
instance Eq Dimension where
  a@(Dimension ah aw) == b@(Dimension bh bw) = ah == bh && aw == bw
  a@(Dimension ah aw) /= b@(Dimension bh bw) = ah /= bh || aw /= bw

parseWindow :: Maybe (Size.Window Int) -> Dimension
parseWindow (Nothing) = (Dimension {height=0, width=0})
parseWindow (Just (Size.Window {Size.height = h, Size.width = w}))
  | and [w > 80, h > 40]                = (Dimension {height=h, width=w})
  | otherwise                           = (Dimension {height=0, width=0})

isCompatibleWindow :: Dimension -> Bool
isCompatibleWindow (Dimension h w) = not $ h==w && h==0


clearScreenToBegin :: (X, Y) -> IO()
clearScreenToBegin (0, 0) = putStr ""
clearScreenToBegin (x, y) = ANSI.setCursorPosition x y >> ANSI.clearFromCursorToScreenBeginning


textFinish :: (X, Y) -> String -> IO ()
textFinish (xpoint, ypoint) message = do
  let defaultUserShip = []
        ++ ["       /#\\       "]
        ++ ["    \\ / | \\ /    "]
        ++ ["    /\\\\ | //\\    "]
        ++ ["       - -       "]
  let bueno = ["--{ Press any key }--"]
  let nmsg  = ["--{ "++message++" }--"]
  let end =    ["--{ EOF }--"]
  let _000L = 0
  let _001L = (+) (length defaultUserShip) 6
  let _002L = (+) _001L 2
  let _003L = (+) _002L 2
  let _000H = 0
  let _001H = (div (length $ head defaultUserShip) 2) - (div (length $ head nmsg) 2)
  let _002H = (div (length $ head defaultUserShip) 2) - (div (length $ head bueno) 2)
  let _003H = (div (length $ head defaultUserShip) 2) - (div (length $ head end) 2)
  drowing   ANSI.Cyan        _000H       _000L      defaultUserShip
  drowing   ANSI.White       _001H       _001L      nmsg
  drowing   ANSI.White       _002H       _002L      bueno
  drowing   ANSI.White       _003H       _003L      end
  where
    drowing :: ANSI.Color -> X -> Y -> [String] -> IO ()
    drowing color offsetX offsetY [] = putStr ""
    drowing color offsetX offsetY (str:strings)= do
      ANSI.setCursorPosition (ypoint+offsetY) (xpoint+offsetX)
      colorize str color
      drowing color offsetX (offsetY+1) strings
    colorize :: String -> ANSI.Color -> IO ()
    colorize text color = do
      ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
      putStr text
      ANSI.setSGR [ANSI.Reset]


textIntro :: (X, Y) -> IO ()
textIntro (xpoint, ypoint) = do
  let space = []
        ++ ["  ____  "]
        ++ [" / ___| _ __   __ _  ___ ___ "]
        ++ [" \\___\\ | '_ \\ / _` |/ __/ _ \\ "]
        ++ ["  ___) | |_) | (_| | (_|  __/  "]
        ++ [" |____/| .__/ \\__,_|\\___\\___| "]
        ++ ["       |_| "]
  let invaders = []
        ++ ["  ___                     _                "]
        ++ [" |_ _|_ ____   ____ _  __| | ___ _ __ ___  "]
        ++ ["  | || '_ \\ \\ / / _` |/ _` |/ _ \\ '__/ __| "]
        ++ ["  | || | | \\ V / (_| | (_| |  __/ |  \\__ \\ "]
        ++ [" |___|_| |_|\\_/ \\__,_|\\__,_|\\___|_|  |___/ "]
  let by =              ["by"]
  let authorA =         ["    SerhiiRi"]
  let authorB =         ["    Morfeu5z"]
  let hint1 =           ["    Shoot [J] Moving [A]-[D]    "]
  let p1 =              ["--{ press A to Start the Game }--"]
  let p2 =              ["     --{ or D for Leave }--      "]
  let p3 =              ["          --{ EOF }--            "]
  let _000L   = 0
  let _001L   = length space
  let _002L   = (+) (_001L + 2) $ length invaders
  let _003L   = (+) _002L 1
  let _004L   = (+) _003L 1
  -- real menu
  let _015L   = (+) _004L 5
  let _005L   = (+) _015L 2
  let _006L   = (+) _005L 2
  let _007L   = (+) _006L 2
  let _001H   = (length $ head invaders) - (length $ head authorA) -1
  let _002H   = (div (length $ head invaders) 2) - (div (length $ head p1) 2)
  drowing   ANSI.Red         6       _000L      space
  drowing   ANSI.Cyan        0       _001L      invaders
  drowing   ANSI.Red         _001H   _002L      by
  drowing   ANSI.Cyan        _001H   _003L      authorA
  drowing   ANSI.Cyan        _001H   _004L      authorB
  drowing   ANSI.Red         _002H   _015L      hint1
  drowing   ANSI.White       _002H   _005L      p1
  drowing   ANSI.White       _002H   _006L      p2
  drowing   ANSI.White       _002H   _007L      p3
  where
    drowing :: ANSI.Color -> X -> Y -> [String] -> IO ()
    drowing color offsetX offsetY [] = putStr ""
    drowing color offsetX offsetY (str:strings)= do
      ANSI.setCursorPosition (ypoint+offsetY) (xpoint+offsetX)
      colorize str color
      drowing color offsetX (offsetY+1) strings
    colorize :: String -> ANSI.Color -> IO ()
    colorize text color = do
      ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
      putStr text
      ANSI.setSGR [ANSI.Reset]
