{-# LANGUAGE MultiWayIf #-}
-- importing library to working with ANSI console
import System.Console.ANSI as ANSI
import System.Console.Terminal.Size
import System.IO
import System.Posix.Unistd
import System.Environment
import System.Exit
import Control.Exception
import qualified Data.Maybe as TMaybe (isNothing, isJust, fromJust)
import qualified Library.Vector         as Vector
import qualified Library.Ships          as Ships
import qualified System.Random          as Random (StdGen, mkStdGen, randomR)


data Setting = Setting { health :: Int
                       , offset :: Int
                       , score  :: Int
                       , window :: Vector.Dimension
                       , rndgen :: Random.StdGen
                       , file_lifes     :: FilePath
                       , file_bright    :: FilePath
                       , file_key       :: FilePath
                       } deriving (Show)


data LoopSetting = LoopSetting { counter    :: Int
                               , cospi      :: Float
                               , track      :: Int
                               , down       :: Int
                               } deriving (Show)

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

keyboardController :: Vector.Dimension -> Maybe Char -> Ships.Ship -> Maybe Ships.Ship
keyboardController (Vector.Dimension xhei xwid) myChar gamerShip
        | myChar == Just 'a'   = Just ( (Ships.point gamerShip) `inSection` (subtract 2))
        | myChar == Just 'd'   = Just ( (Ships.point gamerShip) `inSection` (+2))
        | myChar == Nothing    = Just ( (Ships.point gamerShip) `inSection` (id))
        | myChar == Just 'j'   = Ships.addBulletStack gamerShip
        | otherwise            = Just ( (Ships.point gamerShip) `inSection` (id))
        where
          inSection (Vector.Point newX newY) xFun = gamerShip { Ships.point = (
                                                                  if ((xFun $ newX) > -2) && (((xFun $ newX)+10) < xwid)
                                                                  then (Vector.Point (xFun newX) newY)
                                                                  else (Vector.Point newX newY)
                                                                  )
                                                              }

finishGame :: String -> Setting -> IO ()
finishGame message _SETTING = do
  putStr ANSI.clearFromCursorToScreenBeginningCode
  putStr ANSI.clearFromCursorToScreenEndCode
  window <- Vector.parseWindow <$> size
  ANSI.setCursorPosition ((Vector.height window) `div` 2) (((Vector.width window) `div` 2)  -  (div (length message) 2))
  putStrLn message
  ANSI.setCursorPosition ((+) 1 $ (Vector.height window) `div` 2) (((Vector.width window) `div` 2)  -  (div (length message) 2))
  putStrLn $ "Score: " ++ (show $ score _SETTING)
  usleep 100000
  finishGame message _SETTING



mainLoop :: LoopSetting -> Setting -> Maybe Ships.Ship -> [Maybe Ships.Ship] -> IO ()
mainLoop _LOOP _SETTING userShip enemyShips = do
  let iterator                  = counter       _LOOP
  let _PI                       = cospi         _LOOP
  let downOffset                = down          _LOOP
  let downTrack                 = track         _LOOP

  let windowOffset              = offset        _SETTING
  let windowDimension           = window        _SETTING
  let randomGenerator           = rndgen        _SETTING
  rescanWindowsDimension        <- Vector.parseWindow <$> size
  ANSI.setCursorPosition 0 0
  hFlush stdout
  --char <- ifReadyDo stdin getChar
  char <-((readFile (file_key _SETTING)) >>= (\x -> return $ head x))
  let charM = if (mod iterator 3 == 0 ) then (Just char) else (Nothing)
  let (randomValue, newRandomGenerator) = Random.randomR (1, 1000) randomGenerator :: (Int, Random.StdGen)
  let nnewShip =
        userShip
        >>= (keyboardController windowDimension charM)
        >>= Ships.runAndCleanBullet 0 (subtract 1) (0 <)
        >>= Ships.intersectEnemyShips enemyShips
  let ewEnemyShips =
        (\x -> x >>= Ships.runAndCleanBullet (mod iterator 3) (+1) (< (Vector.height windowDimension)))

        <$> (Ships.addBulletOnEnemyStack (randomValue > 980) newRandomGenerator enemyShips)
  let (new_PI, new_down, nnewEnemyShips) =
        if (mod iterator (compr downTrack downOffset windowOffset) == 0)
        then
           if (let (l, r) = Ships.getShipsLimitersPoint ewEnemyShips; (wl, wr) = (0, Vector.width windowDimension)
               in (or [wl==l, wr==r]))

           then (let np = _PI + pi in (np, downOffset+1, Ships.moveEnemyShips (+ (round $ cos np)) (+1) True ewEnemyShips))
           else (_PI, downOffset, Ships.moveEnemyShips (+ (round $ cos _PI)) (id) True ewEnemyShips)
        else (_PI, downOffset, ewEnemyShips)
  let (newShip, newEnemyShips, toEreasing)  = Ships.killing nnewEnemyShips nnewShip

  Ships.renderShipM             newShip
  Ships.renderGamerBullets      newShip
  Ships.renderBulletCountM      newShip
  Ships.renderLifeCountM        newShip rescanWindowsDimension
  Ships.clearBetweenEnemyShips  enemyShips
  Ships.clearEnemyShips         toEreasing
  Ships.renderEnemyBullets      newEnemyShips rescanWindowsDimension
  Ships.renderAllEnemyShips     newEnemyShips


  writeFile  (file_lifes _SETTING) (if (TMaybe.isJust newShip)
                                    then(show $ Ships.lifes $ TMaybe.fromJust newShip)
                                    else "0")

  ANSI.setCursorPosition 1 25
  bright <- readFile      (file_bright _SETTING)
  putStr $ "Bright:" ++ bright

  ANSI.setCursorPosition 1 45
  key <- readFile      (file_key _SETTING)
  putStr $ "Presd:" ++ key


  let n_LOOP = _LOOP { down=new_down, cospi=new_PI, counter=(if | iterator == 10000  -> 0 | otherwise -> iterator +1) }
  usleep 10000
  case () of
    _ | and [TMaybe.isJust newShip, not $ null newEnemyShips]-> (mainLoop n_LOOP (_SETTING {window=(rescanWindowsDimension), rndgen=newRandomGenerator}) newShip newEnemyShips)
    _ | and [TMaybe.isNothing newShip, not $ null newEnemyShips] -> finishGame "You lose! ha-ha!" _SETTING
    _ | and [TMaybe.isJust newShip, null newEnemyShips ] -> ANSI.clearScreen >> level (_SETTING { health=(Ships.lifes (TMaybe.fromJust newShip))
                                                                                                   , offset=((offset _SETTING) + 1)
                                                                                                   , score=((score _SETTING) + 10)
                                                                                                   , window=rescanWindowsDimension
                                                                                                   , rndgen=newRandomGenerator })
    _           -> finishGame "Crashed game" _SETTING
  where
    compr limiter oa ob = let speed = (limiter - oa - ob) in if speed<1 then 1 else speed

level :: Setting -> IO ()
level startUpSetting = do
  windowDimension        <- Vector.parseWindow <$> size
  let userShips = Just Ships.Ship {
        Ships.point             = genr windowDimension
        , Ships.viewShip        = Ships.defaultUserShip
        , Ships.shipSprite      = Ships.defaultUserSprite
        , Ships.viewBullet      = Ships.defaultBulletView
        , Ships.bullets         = []
        , Ships.lifes           = health startUpSetting
        }
  let loopSetting = LoopSetting { counter       = 0
                                , cospi         = pi
                                , down          = 0
                                , track         = 12}
  let enemyMatrix               = concat $ Ships.createEnemyShipTemplate (offset startUpSetting) windowDimension
  mainLoop loopSetting startUpSetting userShips enemyMatrix
  where
    genr (Vector.Dimension {Vector.height=xheight, Vector.width=xwidth})
      = (Vector.Point ((xwidth `div` 2)-10) (xheight - (xheight `div` 6)))


main = do
  -- arg <- getArgs
  -- if (length arg < 3) then exitSuccess
  --   else putStr ""
  -- let fileLifes         = arg !! 0
  -- let fileBright        = arg !! 1
  -- let fileKey           = arg !! 2

  putStr ANSI.hideCursorCode
  ANSI.clearScreen
  old <-hGetEcho stdin
  hSetEcho stdin False
  let startSettings = Setting { offset=0
                              , health=3
                              , score=0
                              , window=(Vector.Dimension 0 0)
                              , rndgen=(Random.mkStdGen 12341234)
                              , file_lifes="./lifes"
                              , file_bright="./bright"
                              , file_key="./key"
                              }
  catch (level startSettings) handler
  putStrLn ANSI.showCursorCode
  putStr ANSI.clearFromCursorToScreenEndCode
  putStr ANSI.clearFromCursorToScreenBeginningCode
  ANSI.setCursorPosition 0 0
  where
    handler :: SomeException -> IO ()
    handler e = putStrLn $ ANSI.showCursorCode


type ErrorMessage = [String]
printHelp :: IO ()
printHelp = putStrLn $ "usage: si\n"
          ++ "Space Invader the console emulator of Atari game.\n"
          ++ "list of arguments:\n"
          ++ "\thelp\t\tshow this message\n"
          ++ "\t/<path>/lifes\t\toutput game file"
          ++ "\t/<path>/bright\t\tinput file for ship color"
          ++ "\t/<path>/key\t\tsystem for replace with keyboard interruption"




printError :: ErrorMessage -> IO ()
printError errorList = putStrLn $ " ** Error * "
                     ++ (concat $ map ("\n -> " ++ ) errorList)
                     ++ " -* end *- "
