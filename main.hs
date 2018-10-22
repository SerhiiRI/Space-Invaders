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


returnInWidth' :: Vector.Dimension -> Maybe Char -> Ships.GamerShip -> Maybe Ships.GamerShip
returnInWidth' (Vector.Dimension xhei xwid) myChar gamerShip
        | myChar == Just 'a'   = Just ( (Ships.point gamerShip) `inSection` (subtract 2))
        | myChar == Just 'd'   = Just ( (Ships.point gamerShip) `inSection` (+2))
        | myChar == Nothing    = Just ( (Ships.point gamerShip) `inSection` (id))
        | otherwise            = Nothing
        where
          inSection (Vector.Point newX newY) xFun = gamerShip { Ships.point = (
                                                                  if ((xFun $ newX) > -2) && (((xFun $ newX)+10) < xwid)
                                                                  then (Vector.Point (xFun newX) newY)
                                                                  else (Vector.Point newX newY)
                                                                  )
                                                              }

mainLoopIO :: Vector.Dimension -> Maybe Ships.GamerShip -> IO ()
mainLoopIO _ Nothing = putStrLn "ERROR"
mainLoopIO windowDimension userShip = do
  -- IO functionality
  hFlush stdout
  char <- ifReadyDo stdin getChar -- (bracket_ (hSetEcho stdin False) (hSetEcho stdin old) getChar)

  let newShip = userShip >>= (returnInWidth' windowDimension char)
  -- let usrShip = returnInWidth' windowDimension char userShip
  Ships.renderShip' newShip

  usleep 5000
  mainLoopIO windowDimension newShip



main = do
  putStr ANSI.hideCursorCode
  ANSI.clearScreen
  old                    <- hGetEcho stdin
  hSetEcho stdin False
  windowDimension        <- Vector.parseWindow <$> size
  let userShips = Just Ships.GamerShip {
        Ships.point             = genr windowDimension
        , Ships.viewShip        = Ships.defaultUserShip
        , Ships.shipSprite      = Ships.defaultUserSprite
        , Ships.viewBullet        = Ships.defaultBulletView
        , Ships.bullets         = []
        , Ships.lifes           = Just 3
        }
  mainLoopIO windowDimension userShips
  putStr ANSI.showCursorCode
  where
    genr ( Vector.Dimension {Vector.height=xheight, Vector.width=xwidth}) = (Vector.Point ((xwidth `div` 2)-10) (xheight - (xheight `div` 6)))

