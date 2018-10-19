-- importing library to working with ANSI console
import System.Console.ANSI as ANSI
import System.Console.Terminal.Size
import System.IO
import Control.Exception


type CurrentCoordinate = Int

data Dimension = Dimension { hei :: Int
                           , wid :: Int
                           } deriving (Show)

data Coordinate = Coordinate { x :: Int
                             , y :: Int
                             } deriving (Show)


userShip :: [String]
userShip = [ "     /#\\     ", "  \\ / | \\ /  ", "  /\\\\ | //\\  " ,"     - -     " ]

parseWindow :: Maybe (Window Int) -> Dimension
parseWindow (Just (Window {height = h, width = w})) = (Dimension {hei=h, wid=w})
parseWindow (Nothing) = Dimension {hei=0, wid=0}


-- You may add X argumnet and line to rendering
-- TODO: create spcefic data type to each of types
makingBashPointRow :: Coordinate -> [String] -> IO ()
makingBashPointRow (Coordinate {x=x_position, y=y_position}) (x:xs) = do
  ANSI.setCursorPosition y_position x_position
  putStrLn x
  makingBashPointRow (Coordinate {x=x_position, y=(y_position+1)}) xs
makingBashPointRow (Coordinate {x=x_position, y=y_position}) [] = do
  ANSI.setCursorPosition y_position x_position

{-
ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing
-}

returnInWidth :: Dimension -> Coordinate -> Char -> Coordinate
returnInWidth (Dimension {hei=xhei, wid=xwid}) (Coordinate {x=x1,y=y1}) myChar
        | myChar == 'a'   = inDimension (Coordinate {x=(x1-2), y=y1})
        | myChar == 'd'   = inDimension (Coordinate {x=(x1+2), y=y1})
        -- | myChar == 'w'   = inDimension (Coordinate {x=x1, y=y1})
        -- | myChar == 's'   = inDimension (Coordinate {x=x1, y=y1})
        | otherwise       = (Coordinate {x=x1, y=y1})
        where inDimension (Coordinate {x=newX, y=newY}) = if (newX) > 0 || (newX+10) < xwid then
                return (Coordinate newX y1)
                else return (Coordinate {x=x1, y=y1})


mainLoopIO :: Coordinate -> IO Int
mainLoopIO currentPosition = do
  windowDimension        <- parseWindow <$> size
  hFlush stdout
  old                    <- hGetEcho stdin
  kb_01                  <- bracket_ (hSetEcho stdin False) (hSetEcho stdin old) getChar
  let changedWindowsPointCoordinate = returnInWidth windowDimension currentPosition kb_01
  makingBashPointRow changedWindowsPointCoordinate userShip
  mainLoopIO changeWidowsPointCoordinate


main = do
  --putStr ANSI.hideCursorCode
  let xCoordinate = (Coordinate {x=1, y=15})
  mainLoopIO xCoordinate
  --putStr ANSI.showCursorCode


