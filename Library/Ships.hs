module Library.Ships
  ( Bullet(Bullet)
  , GamerShip(..)
  --, EnemyShip(EnemyShip,EmptyShip)
  , defaultUserShip
  , defaultBulletView
  , defaultUserSprite
  , renderShipM
  , renderShip
  , addBulletStack
  , renderBulletsM
  , renderBullets
  , renderBulletCountM
  , renderLifeCountM
  , runBullet
  , runAndCleanBullet
  ) where

-- Point (X, Y)
import Library.Vector
import System.Console.ANSI as ANSI

defaultUserShip :: [String]
defaultUserShip = [ "     /#\\     ", "  \\ / | \\ /  ", "  /\\\\ | //\\  " ,"     - -     " ]

defaultUserSprite :: (Width, Height)
defaultUserSprite = (length(head defaultUserShip), length(defaultUserShip))

defaultBulletView :: [String]
defaultBulletView = ["^","!"," "]


type Width = Int
type Height = Int


-- | Bullet data types is only Point
newtype Bullet = Bullet { getBullet :: Point } deriving (Show)


-- | GamerShip - main controll user ship
-- This ships may be abble to interraction
data GamerShip = GamerShip { point           :: Point
                           , viewShip        :: [String]
                           , shipSprite      :: (Width, Height)
                           , viewBullet      :: [String]
                           , bullets         :: [Bullet]
                           , lifes           :: Maybe Int
                           } deriving (Show)




renderShipM :: Maybe GamerShip -> IO ()
renderShipM (Nothing) = do
  putStrLn ""
renderShipM (Just ship@(GamerShip { point=(Point x_position y_position), viewShip=(l:ls) })) = do
  ANSI.setCursorPosition y_position x_position
  putStrLn l
  renderShipM (Just (ship {point=(Point x_position (y_position+1)), viewShip=ls}))
renderShipM (Just (GamerShip {point=(Point x_position y_position), viewShip=[]})) = do
  ANSI.setCursorPosition y_position x_position


renderShip :: GamerShip -> IO ()
renderShip ship@(GamerShip { point=(Point x_position y_position), viewShip=(l:ls) }) = do
  ANSI.setCursorPosition y_position x_position
  putStrLn l
  renderShip (ship {point=(Point x_position (y_position+1)), viewShip=ls})
renderShip (GamerShip {point=(Point x_position y_position), viewShip=[]}) = do
  ANSI.setCursorPosition y_position x_position



addBulletStack :: GamerShip -> Maybe GamerShip
addBulletStack ship@(GamerShip {point=(Point xloc yloc), shipSprite=(spriteW,spriteH), bullets=bulletList }) =
  Just (ship {bullets=(newBulletPoint : bulletList)})
  where
    newBulletPoint = Bullet (Point ((div spriteW 2) + xloc) (yloc - 1))



renderBullets :: GamerShip -> IO ()
renderBullets ship@(GamerShip {bullets=[]}) = putStrLn ""
renderBullets ship@(GamerShip {viewBullet=vb, bullets=(b:bx)}) = do
  rBullet vb b
  renderBullets (ship {bullets=bx})
  where rBullet (vv:vvb) bulletPoint@(Bullet {getBullet=(Point x_position y_position)}) = do
          ANSI.setCursorPosition y_position x_position
          putStrLn vv
          if not (null vvb)
            then ANSI.setCursorPosition y_position x_position
            else rBullet vvb (Bullet (Point{x=x_position, y=(y_position+1)}))


renderBulletsM :: Maybe GamerShip -> IO ()
renderBulletsM Nothing = putStrLn ""
renderBulletsM (Just ship@(GamerShip {bullets=[]})) = putStrLn ""
renderBulletsM (Just ship@(GamerShip {viewBullet=vb, bullets=blts@(b:bx)})) = do
  let finishRender = [" ", " "]
  rBullet vb b
  renderBulletsM (Just (ship {bullets=bx}))
  where rBullet (vv:vvb) bulletPoint@(Bullet {getBullet=(Point x_position y_position)}) = do
          ANSI.setCursorPosition y_position x_position
          if (y_position < 2)
            then putStrLn " "
            else putStrLn vv
          if (null vvb)
            then ANSI.setCursorPosition y_position x_position
            else rBullet vvb (Bullet (Point {x=x_position, y=(y_position +1)}))

renderBulletCountM :: Maybe GamerShip -> IO ()
renderBulletCountM Nothing = putStrLn ""
renderBulletCountM (Just ship) = do
  ANSI.setCursorPosition 1 5
  putStrLn $ (++) "Bullts: " (show $ length (bullets ship))


renderLifeCountM :: Maybe GamerShip -> Dimension -> IO ()
renderLifeCountM Nothing _ = putStrLn ""
renderLifeCountM (Just ship@(GamerShip {lifes=(Just countLifes)})) terminalWindowsDimension = do
  ANSI.setCursorPosition 1 ((subtract 15) (width terminalWindowsDimension))
  putStrLn $ "Lifies: " ++ (show $ countLifes)





runBullet :: GamerShip -> Maybe GamerShip
runBullet ship@(GamerShip {bullets=[]}) = Just ship
runBullet ship@(GamerShip {bullets=bulletsList}) = Just ship { bullets=( map _MOVED_UP bulletsList)}
  where
    _MOVED_UP (Bullet (Point nx ny)) = (Bullet {getBullet=(Point {x=nx, y=(ny-1)})})


runAndCleanBullet :: GamerShip -> Maybe GamerShip
runAndCleanBullet ship@(GamerShip {bullets=[]}) = Just ship
runAndCleanBullet ship@(GamerShip {bullets=bulletsList}) = Just ship { bullets=( foldr cleanAndMove [] bulletsList)}
  where
    cleanAndMove (Bullet (Point nx ny)) newBulletsList
      | ((ny) > 0) = (Bullet {getBullet=(Point {x=nx, y=(ny-1)})}) : newBulletsList
      | otherwise = (newBulletsList)





