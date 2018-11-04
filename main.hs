-- importing library to working with ANSI console
import System.Console.ANSI as ANSI
import System.Console.Terminal.Size
import System.IO
import System.Posix.Unistd
import Control.Exception
import qualified Library.Vector         as Vector
import qualified Library.Ships          as Ships



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



mainLoopIO :: Vector.Dimension -> Maybe Ships.Ship -> [[Ships.Ship]] -> IO ()
mainLoopIO _ Nothing _ = putStrLn "ERROR"
mainLoopIO windowDimension userShip enemyShips = do
  -- IO functionality
  hFlush stdout
  char <- ifReadyDo stdin getChar -- (bracket_ (hSetEcho stdin False) (hSetEcho stdin old) getChar)

  let newShip =
        userShip
        >>= (keyboardController windowDimension char)
        -- TODO: >>= Ships.killedByEnemyBullet enemyShips
        >>= Ships.runAndCleanBullet
  {- TODO: implement all this function
  let newEnemyShips =
        enemyShips
        >>= Ships.killedByUserBullet newShip
        >>= Ships.moveEnemyShips
        >>= Ships.runAndCleanBullet
  -}
  Ships.renderShipM             newShip
  Ships.renderBulletsM          newShip
  Ships.renderBulletCountM      newShip
  Ships.renderLifeCountM        newShip windowDimension
  Ships.renderAllEnemyShips     enemyShips --newEnemyShips
  -- TODO: Ships.renderAllEnemyBullets   newEnemyShips
  usleep 10000

  mainLoopIO windowDimension newShip enemyShips

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
        , Ships.viewBullet      = Ships.defaultBulletView
        , Ships.bullets         = []
        , Ships.lifes           = Just 3
        }
  let enemyMatrix = Ships.createEnemyShipTemplate windowDimension
  catch (mainLoopIO windowDimension userShips enemyMatrix) handler
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

