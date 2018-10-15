-- implementing string row ships for linier
-- rendering in recursive output loop,
-- UserShip
import System.Console.ANSI


userShip :: [String]
userShip = [ "   /#\\   ", "\\ / | \\ /", "/\\\\ | //\\" ,"   - -   " ]


-- You may add X argumnet and line to rendering
-- TODO: create spcefic data type to each of types
makingBashPointRow :: (Show a) => a -> String -> String
makingBashPointRow x_position line = "\\e[20;" ++ (show x_position) ++ "H" ++ line


drowingShip :: Int -> String
drowingShip x_position
  | x_position < 0 = "DEC_ERROR"
  | x_position > 30 = "INC_ERROR"
  | otherwise = foldr (\coordinate acumulator-> makingBashPointRow x_position coordinate) "" userShip
  -- | otherwise = map (makingBashPointRow x_position) userShip


inf :: IO a
inf = do
  x <- read <$> getLine
  print $ drowingShip x
  inf

main = inf



-- (define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)
-- (setq x-select-enable-clipboard t)
