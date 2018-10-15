-- importing library to working with ANSI console
import System.Console.ANSI as ANSI
import System.Console.Terminal.Size
import System.IO


-- userShip, provides as a test, in real situation:
-- TODO: creating User Ships must be procedural in the separate module
userShip :: [String]
userShip = [ "    /#\\    ", " \\ / | \\ / ", " /\\\\ | //\\ " ,"    - -    " ]


-- parsing window for geting windows width char size
-- TODO: change geting tuple for geting demention
-- using for parsing "size" command library:
---- System.Console.Terminal.Size
parseWindow :: (Integral n) => Maybe (Window n) -> n
parseWindow (Just (Window {height = h, width = w})) = w
parseWindow (Nothing) = 0


-- You may add X argumnet and line to rendering
-- TODO: create spcefic data type to each of types
makingBashPointRow :: Int -> Int -> [String] -> IO ()
makingBashPointRow y_position x_position (x:xs) = do
  ANSI.setCursorPosition y_position x_position
  putStrLn x
  makingBashPointRow (y_position+1) x_position xs
makingBashPointRow y_position x_position [] = do
  ANSI.setCursorPosition y_position x_position




getValue :: Int -> Char -> Maybe Int
getValue value 'a' = Just (value - 1)
getValue value 'd' = Just (value + 2)
getValue value _ = Nothing





ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing





returnInWidth :: Int -> Maybe Int -> Int
returnInWidth width Nothing = width
returnInWidth width (Just getPositionX)
        | getPositionX > width          = width - 11
        | getPositionX < 0              = 1
        | otherwise                     = getPositionX





-- Infinite loop for main function
-- Function get kode symbol XB86_* and
-- render `userShip` in spectial parsing point
mainLoopIO :: Int -> IO Int
mainLoopIO xv = do
  widthOfWindows <- parseWindow <$> size                        -- widthOfWindows = 69 :: IO
  pointOnWindows <- fmap (getValue xv) getChar                  -- 
  let xPoint = returnInWidth widthOfWindows pointOnWindows
  --mapM_ putStrLn userShip
  --mapM_ (makingBashPointRow xPoint) userShip
  makingBashPointRow 50 xPoint userShip
  mainLoopIO xv


main = do
  putStr ANSI.hideCursorCode
  let xCoordinate = 30
  mainLoopIO 30
  putStr ANSI.showCursorCode









