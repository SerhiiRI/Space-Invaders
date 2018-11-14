-- importing library to working with ANSI console
import System.Console.ANSI as ANSI
import System.Console.Terminal.Size
import System.IO
import System.Posix.Unistd
import Control.Exception
import qualified Library.Vector         as Vector
import qualified Library.Ships          as Ships
import qualified System.Random          as Random (StdGen, mkStdGen, randomR)


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


finishGame :: IO ()
finishGame = do
  ANSI.setCursorPosition 20 20
  putStrLn "No zjebaleś kolego"
  usleep 100000
  finishGame

mainLoopIO :: Float -> Int -> Vector.Dimension -> Random.StdGen-> Maybe Ships.Ship -> [Maybe Ships.Ship] -> IO ()
mainLoopIO _ _ _ _ Nothing _ = putStrLn "ERROR"
mainLoopIO _PI counter windowDimension randomGenerator userShip enemyShips = do
  ANSI.setCursorPosition 0 0
  -- IO functionality
  hFlush stdout
  char <- ifReadyDo stdin getChar -- (bracket_ (hSetEcho stdin False) (hSetEcho stdin old) getChar)
  let (randomValue, newRandomGenerator) = Random.randomR (1, 1000) randomGenerator :: (Int, Random.StdGen)
  let newShip =
        userShip
        >>= (keyboardController windowDimension char)
        -- TODO: >>= Ships.killedByEnemyBullet enemyShips
        >>= Ships.runAndCleanBullet 0 (subtract 1) (0 <)
  --TODO: implement all this function
  --let newEnemyShips =
  --      enemyShips
  --      [X] Ships.addBulletOnEnemyStack (randomValue > 40) newRandomGenerator
  --      [ ] Ships.killedByUserBullet newShip
  --      [ ] Ships.moveEnemyShips
  --      [X] Ships.runAndCleanBullet (1+) (Vector.height windowDimension >)


  let ewEnemyShips =
        (\x -> x >>= Ships.runAndCleanBullet (mod counter 4) (+1) (< (Vector.height windowDimension)))
        <$> (Ships.addBulletOnEnemyStack (randomValue > 990) newRandomGenerator enemyShips)
  let (new_PI, newEnemyShips) =
        if (mod counter 40 == 0)
        then
           if (let (l, r) = Ships.getShipsLimitersPoint ewEnemyShips; (wl, wr) = (0, Vector.width windowDimension)
               in (or [wl==l, wr==r]))
           then (let np = _PI + pi in (np, Ships.moveEnemyShips (+ (round $ cos np)) (+1) True ewEnemyShips))
           else (_PI, Ships.moveEnemyShips (+ (round $ cos _PI)) (id) True ewEnemyShips)
        else (_PI, ewEnemyShips)

  Ships.renderShipM             newShip
  Ships.renderGamerBullets      newShip
  Ships.renderBulletCountM      newShip
  Ships.renderLifeCountM        newShip windowDimension
  Ships.renderEnemyBullets      newEnemyShips windowDimension
  Ships.renderAllEnemyShips     newEnemyShips
  Ships.renderEnemyBulletCount  newEnemyShips
  Ships.clearBetweenEnemyShips  newEnemyShips
  let nc = if | counter == 10000  -> 0
              | otherwise         -> counter+1
  -- TODO: if intersect with ships,
  -- then run function finishGame
  usleep 10000
  mainLoopIO new_PI nc windowDimension newRandomGenerator newShip newEnemyShips

main = do
  putStr ANSI.hideCursorCode
  ANSI.clearScreen
  old                    <- hGetEcho stdin
  hSetEcho stdin False

  windowDimension        <- Vector.parseWindow <$> size
  let userShips = Just Ships.Ship {
        Ships.point             = genr windowDimension
        , Ships.viewShip        = Ships.defaultUserShip
        , Ships.shipSprite      = Ships.defaultUserSprite
        , Ships.viewpBullet      = Ships.defaultBulletView
        , Ships.bullets         = []
        , Ships.lifes           = Just 3
        }
  -- create one layout enemy matrix [[Ship]] to [Ship]
  let enemyMatrix               = concat $ Ships.createEnemyShipTemplate windowDimension
  let startRandomGenerator      = Random.mkStdGen 980918
  catch (mainLoopIO pi 0 windowDimension startRandomGenerator userShips enemyMatrix) handler
  putStrLn ANSI.showCursorCode

  --ANSI.clearScreen
  --putStr "\ESC[2J"
  putStr ANSI.clearFromCursorToScreenBeginningCode
  putStr ANSI.clearFromCursorToScreenEndCode

  ANSI.setCursorPosition 0 0
  where
    genr ( Vector.Dimension {Vector.height=xheight, Vector.width=xwidth}) = (Vector.Point ((xwidth `div` 2)-10) (xheight - (xheight `div` 6)))
    handler :: SomeException -> IO ()
    handler e = putStrLn $ ANSI.showCursorCode

