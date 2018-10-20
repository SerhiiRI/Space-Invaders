-- importing library to working with ANSI console
import System.Console.ANSI as ANSI
import System.Console.Terminal.Size
import System.IO
import System.Posix.Unistd
import Control.Exception
import qualified Library.Point as Point
import qualified Library.Dimension as Dimension

type CurrentPoint = Int

newtype Bullet = Bullet { getBullet :: (Int, Int) }

userShip :: [String]
userShip = [ "     /#\\     ", "  \\ / | \\ /  ", "  /\\\\ | //\\  " ,"     - -     " ]

parseWindow :: Maybe (Window Int) -> Dimension.Dimension
parseWindow (Just (Window {height = h, width = w})) = (Dimension.Dimension {height=h, width=w})
parseWindow (Nothing) = Dimension.Dimension {height=0, width=0}


-- You may add X argumnet and line to rendering
-- TODO: create spcefic data type to each of types
makingBashPointRow :: Point.Point -> [String] -> IO ()
makingBashPointRow (Point.Point x_position y_position) (x:xs) = do
  ANSI.setCursorPosition y_position x_position
  putStrLn x
  makingBashPointRow (Point.Point x_position (y_position+1)) xs
makingBashPointRow (Point.Point x_position y_position) [] = do
  ANSI.setCursorPosition y_position x_position

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

returnInWidth :: Dimension.Dimension -> Point.Point -> Maybe Char -> Point.Point
returnInWidth (Dimension.Dimension {height=xhei, width=xwid}) (Point.Point {Point.x=x1, Point.y=y1}) myChar
        | myChar == Just 'a'   = inDimension (Point.Point {Point.x=(x1-2), Point.y=y1})
        | myChar == Just 'd'   = inDimension (Point.Point {Point.x=(x1+2), Point.y=y1})
        | myChar == Nothing    = inDimension (Point.Point x1 y1)
        | otherwise       = (Point.Point {Point.x=x1, Point.y=y1})
        where inDimension (Point.Point newX newY) = if ((newX) > -2) && ((newX+10) < xwid) then (Point.Point newX y1) else (Point.Point x1 y1)


mainLoopIO :: Dimension.Dimension -> Point.Point -> IO ()
mainLoopIO windowDimension currentPosition = do
  hFlush stdout

  --kb_01                  <- bracket_ (hSetEcho stdin False) (hSetEcho stdin old) ifReadyDo

  char <- ifReadyDo stdin getChar -- (bracket_ (hSetEcho stdin False) (hSetEcho stdin old) getChar)
  let changedWindowsPoint = returnInWidth windowDimension currentPosition char
  makingBashPointRow changedWindowsPoint userShip
  usleep 5000
  mainLoopIO windowDimension changedWindowsPoint


main = do
  putStr ANSI.hideCursorCode
  ANSI.clearScreen
  old                    <- hGetEcho stdin
  hSetEcho stdin False
  windowDimension        <- parseWindow <$> size
  let xPoint = genr windowDimension
  mainLoopIO windowDimension xPoint
  putStr ANSI.showCursorCode
  where
    genr (Dimension.Dimension{height=xheight, width=xwidth}) = (Point.Point ((xwidth `div` 2)-10) (xheight - (xheight `div` 6)))
