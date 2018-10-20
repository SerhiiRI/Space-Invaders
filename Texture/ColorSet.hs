import qualified Data.Text      as MyText
import qualified Data.Map       as MyMap
import System.IO


data Color a value = FG a value | BG a value | CD a value deriving (Show)



getParameter :: String -> Maybe (MyMap.Map String Int)
getParameter line
  | '=' `elem` line = Just (MyMap.fromList [tempTuple])
  | otherwise = Nothing
  where
    toTuple [a, b] = (a, read b)
    tempTuple = toTuple . take 2 . fmap MyText.unpack $ MyText.splitOn (MyText.pack "=") (MyText.pack line)




{-
load :: FilePath -> (MyMap.Map Int Int)
load path = do
  configFileHandler <- words <$> readFile "Media/vt100colors.txt"
  background <- fmap (\x -> getParameter) configFileHandler
  return MyMap.fromList [(1, 2)]
-}
{-
clasyfikator :: String -> Color a b
clasyfikator string
  | string == "BACKGROUND" =
  | string == "FOREGROUND" = 
  | string == "CODES" = 
-}
