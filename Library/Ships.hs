module Library.Ships
  ( Bullet(Bullet)
  , Ship(..)
  --, Ship(Ship,EmptyShip)
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
  , createEnemyShipTemplate
  , renderAllEnemyShips
  ) where

-- Point (X, Y)
import Library.Vector
import System.Console.ANSI as ANSI

defaultUserShip :: [String]
defaultUserShip         = [ "     /#\\     ", "  \\ / | \\ /  ", "  /\\\\ | //\\  " ,"     - -     " ]
defaultUserSprite :: (Width, Height)
defaultUserSprite       = (length(head defaultUserShip), length(defaultUserShip))
defaultBulletView :: [String]
defaultBulletView       = ["^","!"," "]
defaultShip :: [String]
defaultShip        = [" /^\\ ", " \\^/ "]
defaultEnemySprite :: (Width, Height)
defaultEnemySprite      = (length(head defaultShip), length(defaultShip))
defaultEnemyBulletView :: [String]
defaultEnemyBulletView  = ["$", " "]


type Width = Int
type Height = Int


-- | Bullet data types is only Point
newtype Bullet = Bullet { getBullet :: Point } deriving (Show)


-- | Ship - main controll user ship
-- This ships may be abble to interraction
data Ship = Ship { point           :: Point
                           , viewShip        :: [String]
                           , shipSprite      :: (Width, Height)
                           , viewBullet      :: [String]
                           , bullets         :: [Bullet]
                           , lifes           :: Maybe Int
                           } deriving (Show)




-- TODO: create offset to dimension enemymatrix
createEnemyShipTemplate :: Dimension -> [[Ship]]
createEnemyShipTemplate dimension = do
  let enemyMatrix = replicate 2 $ replicate 11 (Ship {
                                 point          = Point 0 0
                                 , viewShip     = defaultShip
                                 , shipSprite   = defaultEnemySprite
                                 , viewBullet   = defaultEnemyBulletView
                                 , bullets      = []
                                 , lifes        = Just 1
                                 }
                                               )
  let (startX, startY) = generateStartPoint dimension
  updateMatrix startX startY enemyMatrix
  where
    updateMatrix px py enemyShips = zipWith (
      \enemies@(enm:enms) y -> toOffset (py+(snd (shipSprite enm))*y) px enemies
      ) enemyShips [1..]
    generateStartPoint (Dimension {height=xheight, width=xwidth}) = ((xwidth `div` 3), (xheight `div` 10))


toOffset :: Int -> Int -> [Ship] -> [Ship]
toOffset y_offset x_offset []           = []
toOffset y_offset x_offset (e:ex)       = do
  [(e {point=(Point x_offset y_offset)})] ++ (toOffset y_offset ((fst (shipSprite e))+ x_offset) ex)



renderAllEnemyShips :: [[Ship]] -> IO ()
renderAllEnemyShips [] = putStrLn ""
renderAllEnemyShips enemyMatrix = do
  mapM_ renderShip $ concat enemyMatrix



renderShipM :: Maybe Ship -> IO ()
renderShipM (Nothing) = do
  putStrLn ""
renderShipM (Just ship@(Ship { point=(Point x_position y_position), viewShip=(l:ls) })) = do
  ANSI.setCursorPosition y_position x_position
  putStrLn l
  renderShipM (Just (ship {point=(Point x_position (y_position+1)), viewShip=ls}))
renderShipM (Just (Ship {point=(Point x_position y_position), viewShip=[]})) = do
  ANSI.setCursorPosition y_position x_position


renderShip :: Ship -> IO ()
renderShip ship@(Ship { point=(Point x_position y_position), viewShip=(l:ls) }) = do
  ANSI.setCursorPosition y_position x_position
  putStrLn l
  renderShip (ship {point=(Point x_position (y_position+1)), viewShip=ls})
renderShip (Ship {point=(Point x_position y_position), viewShip=[]}) = do
  ANSI.setCursorPosition y_position x_position



addBulletStack :: Ship -> Maybe Ship
addBulletStack ship@(Ship {point=(Point xloc yloc), shipSprite=(spriteW,spriteH), bullets=bulletList }) =
  Just (ship {bullets=(newBulletPoint : bulletList)})
  where
    newBulletPoint = Bullet (Point ((div spriteW 2) + xloc) (yloc - 1))



renderBullets :: Ship -> IO ()
renderBullets ship@(Ship {bullets=[]}) = putStrLn ""
renderBullets ship@(Ship {viewBullet=vb, bullets=(b:bx)}) = do
  rBullet vb b
  renderBullets (ship {bullets=bx})
  where rBullet (vv:vvb) bulletPoint@(Bullet {getBullet=(Point x_position y_position)}) = do
          ANSI.setCursorPosition y_position x_position
          putStrLn vv
          if not (null vvb)
            then ANSI.setCursorPosition y_position x_position
            else rBullet vvb (Bullet (Point{x=x_position, y=(y_position+1)}))


renderBulletsM :: Maybe Ship -> IO ()
renderBulletsM Nothing = putStrLn ""
renderBulletsM (Just ship@(Ship {bullets=[]})) = putStrLn ""
renderBulletsM (Just ship@(Ship {viewBullet=vb, bullets=blts@(b:bx)})) = do
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

renderBulletCountM :: Maybe Ship -> IO ()
renderBulletCountM Nothing = putStrLn ""
renderBulletCountM (Just ship) = do
  ANSI.setCursorPosition 1 5
  putStrLn $ (++) "Bullts: " (show $ length (bullets ship))


renderLifeCountM :: Maybe Ship -> Dimension -> IO ()
renderLifeCountM Nothing _ = putStrLn ""
renderLifeCountM (Just ship@(Ship {lifes=(Just countLifes)})) terminalWindowsDimension = do
  ANSI.setCursorPosition 1 ((subtract 15) (width terminalWindowsDimension))
  putStrLn $ "Lifies: " ++ (show $ countLifes)





runBullet :: Ship -> Maybe Ship
runBullet ship@(Ship {bullets=[]}) = Just ship
runBullet ship@(Ship {bullets=bulletsList}) = Just ship { bullets=( map _MOVED_UP bulletsList)}
  where
    _MOVED_UP (Bullet (Point nx ny)) = (Bullet {getBullet=(Point {x=nx, y=(ny-1)})})


runAndCleanBullet :: Ship -> Maybe Ship
runAndCleanBullet ship@(Ship {bullets=[]}) = Just ship
runAndCleanBullet ship@(Ship {bullets=bulletsList}) = Just ship { bullets=( foldr cleanAndMove [] bulletsList)}
  where
    cleanAndMove (Bullet (Point nx ny)) newBulletsList
      | ((ny) > 0) = (Bullet {getBullet=(Point {x=nx, y=(ny-1)})}) : newBulletsList
      | otherwise = (newBulletsList)





