-- importing library to working with ANSI console
import System.Console.ANSI
import System.Console.Terminal.Size


-- userShip, provides as a test, in real situation:
-- TODO: creating User Ships must be procedural in the separate module
userShip :: [String]
userShip = [ "   /#\\   ", "\\ / | \\ /", "/\\\\ | //\\" ,"   - -   " ]



parseWindow :: (Integral n) => Maybe (Window n) -> n
parseWindow (Just (Window {height = h, width = w})) = w
parseWindow (Nothing) = 0



--printOneLine :: Int -> String -> String
--printOneLine = do
--  setCursorPosition


chujIO :: IO Int
chujIO = do
  suka <- size
  xpoint <- fmap read getLine
  let bliat = returnInWidth (parseWindow suka) xpoint
  print bliat
  chujIO



returnInWidth :: Int -> Int -> Int
returnInWidth width getPositionX
        | getPositionX > width          = width
        | getPositionX < 1              = 1
        | otherwise                     = getPositionX




{--
drawing :: IO a
drawing = do
  xPosition <- if getCursorPosition >>= 

  drawing

main = drawing
--}









