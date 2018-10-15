-- importing library to working with ANSI console
import System.Console.ANSI as ANSI
import System.Console.Terminal.Size


-- userShip, provides as a test, in real situation:
-- TODO: creating User Ships must be procedural in the separate module
userShip :: [String]
userShip = [ "   /#\\   ", "\\ / | \\ /", "/\\\\ | //\\" ,"   - -   " ]


-- parsing window for geting windows width char size
-- TODO: change geting tuple for geting demention
-- using for parsing "size" command library:
---- System.Console.Terminal.Size
parseWindow :: (Integral n) => Maybe (Window n) -> n
parseWindow (Just (Window {height = h, width = w})) = w
parseWindow (Nothing) = 0


-- You may add X argumnet and line to rendering
-- TODO: create spcefic data type to each of types
makingBashPointRow :: Int -> String -> IO ()
makingBashPointRow x_position line = do
  ANSI.setCursorPosition 40 x_position
  putStrLn line



--printOneLine :: Int -> String -> String
--printOneLine = do
--  setCursorPosition


-- Infinite loop for main function
-- Function get kode symbol XB86_* and
-- render `userShip` in spectial parsing point
mainLoopIO :: IO Int
mainLoopIO = do
  widthOfWindows <- size                                -- geting windows size
  pointOnWindows <- fmap read getLine                   -- geting left right code 
  let bliat = returnInWidth (parseWindow widthOfWindows) pointOnWindows
  --mapM_ putStrLn userShip
  mapM_ (makingBashPointRow bliat) userShip
  mainLoopIO



returnInWidth :: Int -> Int -> Int
returnInWidth width getPositionX
        | getPositionX > width          = width - 10
        | getPositionX < 1              = 1
        | otherwise                     = getPositionX




{--
drawing :: IO a
drawing = do
  xPosition <- if getCursorPosition >>= 

  drawing

main = drawing
--}









