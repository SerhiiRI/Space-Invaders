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
  , addBulletOnEnemyStack
  , renderBulletCountM
  , renderEnemyBullets
  , renderEnemyBulletCount
  , renderGamerBullets
  , renderLifeCountM
  , runAndCleanBullet
  , runAndCleanBulletE -- test function to destroy
  , createEnemyShipTemplate
  , renderAllEnemyShips
  ) where

-- Point (X, Y)
import Library.Vector
import System.Console.ANSI as ANSI
import System.Random


defaultUserShip         :: [String]
defaultUserShip         = [ "     /#\\     ", "  \\ / | \\ /  ", "  /\\\\ | //\\  " ,"     - -     " ]
defaultUserSprite       :: (Width, Height)
defaultUserSprite       = (length(head defaultUserShip), length(defaultUserShip))
defaultBulletView       :: [String]
defaultBulletView       = ["^","!"," "]
defaultShip             :: [String]
defaultShip             = [" /^\\ ", " \\^/ "]
defaultEnemySprite      :: (Width, Height)
defaultEnemySprite      = (length(head defaultShip), length(defaultShip))
defaultEnemyBulletView  :: [String]
defaultEnemyBulletView  = ["+", " "]


type Width = Int
type Height = Int


-- | Bullet data types is only Point
newtype Bullet = Bullet { getBullet :: Point } deriving (Show)


-- | Ship - main controll user ship
-- This ships may be abble to interraction
data Ship = Ship { point :: Point
                 , viewShip        :: [String]
                 , shipSprite      :: (Width, Height)
                 , viewBullet      :: [String]
                 , bullets         :: [Bullet]
                 , lifes           :: Maybe Int
                 } deriving (Show)



-- Enemy Ships functions
-- TODO: create offset to dimension enemymatrix
createEnemyShipTemplate :: Dimension -> [[Maybe Ship]]
createEnemyShipTemplate dimension = do
  let enemyMatrix = replicate 4 $ replicate 10 (Ship {
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
      \enemies@(enm:enms) y -> toOffset (py+((+) 1 $ snd (shipSprite enm))*y) px enemies
      ) enemyShips [1..]
    generateStartPoint (Dimension {height=xheight, width=xwidth}) = ((xwidth `div` 3), (xheight `div` 10))


-- | addBulletOnEnemyStack make choise using Bool value about logic of shooting
-- StdGen - generator need to choise one of ships 
addBulletOnEnemyStack :: Bool -> StdGen -> [Maybe Ship] -> [Maybe Ship]
addBulletOnEnemyStack False rGen enemyMatrix = enemyMatrix
addBulletOnEnemyStack True rGen enemyMatrix = do
  let (rv, _)  = randomR (1, (length enemyMatrix)-1) rGen
  let (tright, (x:xs)) = splitAt rv enemyMatrix in (tright ++ [ x >>= addBulletStack ] ++ xs)



toOffset :: Int -> Int -> [Ship] -> [Maybe Ship]
toOffset y_offset x_offset []           = []
toOffset y_offset x_offset (e:ex)       = do
  [(Just (e {point=(Point x_offset y_offset)}))] ++ (toOffset y_offset ((fst (shipSprite e))+ x_offset) ex)



renderAllEnemyShips :: [Maybe Ship] -> IO ()
renderAllEnemyShips [] = putStrLn ""
renderAllEnemyShips enemyMatrix = do
  mapM_ renderShipM enemyMatrix


-- render enemy Bullets 

renderShipM :: Maybe Ship -> IO ()
renderShipM (Nothing) = do
  putStr ""
renderShipM (Just ship@(Ship { point=(Point x_position y_position), viewShip=(l:ls) })) = do
  ANSI.setCursorPosition y_position x_position
  putStr l
  renderShipM (Just (ship {point=(Point x_position (y_position+1)), viewShip=ls}))
renderShipM (Just (Ship {point=(Point x_position y_position), viewShip=[]})) = do
  ANSI.setCursorPosition y_position x_position


renderShip :: Ship -> IO ()
renderShip ship@(Ship { point=(Point x_position y_position), viewShip=(l:ls) }) = do
  ANSI.setCursorPosition y_position x_position
  putStr l
  renderShip (ship {point=(Point x_position (y_position+1)), viewShip=ls})
renderShip (Ship {point=(Point x_position y_position), viewShip=[]}) = do
  ANSI.setCursorPosition y_position x_position



addBulletStack :: Ship -> Maybe Ship
addBulletStack ship@(Ship {point=(Point xloc yloc), shipSprite=(spriteW,spriteH), bullets=bulletList }) =
  Just (ship {bullets=(newBulletPoint : bulletList)})
  where
    newBulletPoint = Bullet (Point ((div spriteW 2) + xloc) (yloc))


renderBulletsME :: (Int -> Bool) -> (Int -> Int) -> Maybe Ship -> IO ()
renderBulletsME _ _ Nothing = putStr ""
renderBulletsME _ _ (Just ship@(Ship {bullets=[]})) = putStr ""
renderBulletsME borderPredicate incFunc (Just ship@(Ship {viewBullet=vb, bullets=blts@(b:bx)})) = do
  rBullet vb b
  renderBulletsME borderPredicate incFunc (Just (ship {bullets=bx}))
  where rBullet (vv:vvb) bulletPoint@(Bullet {getBullet=(Point x_position y_position)}) = do
          ANSI.setCursorPosition y_position x_position
          if (borderPredicate y_position)
            then putStr " "
            else putStr vv
          if (null vvb)
            then ANSI.setCursorPosition y_position x_position
            else rBullet vvb (Bullet (Point {x=x_position, y=(incFunc y_position)}))


renderGamerBullets :: Maybe Ship -> IO ()
renderGamerBullets ships = renderBulletsME (< 2) (+1) ships

renderEnemyBullets :: [Maybe Ship] -> Dimension -> IO ()
renderEnemyBullets [] _ = putStr ""
renderEnemyBullets enemyMatrix dimension
  = mapM_ (renderBulletsME ( > (height dimension -1)) (subtract 1)) enemyMatrix


-- | runAndCleanBullet - function
runAndCleanBulletE :: Ship -> Maybe Ship
runAndCleanBulletE ship@(Ship {bullets=[]}) = Just ship
runAndCleanBulletE ship@(Ship {bullets=bulletsList})
  = Just ship { bullets=( foldr cleanAndMove [] bulletsList)}
  where
    cleanAndMove (Bullet (Point nx ny)) newBulletsList
      | (58 > ny) && (1 < ny) = (Bullet {getBullet=(Point {x=nx, y=(1+ny)})}) : newBulletsList
      | otherwise = (newBulletsList)


renderBulletCountM :: Maybe Ship -> IO ()
renderBulletCountM Nothing = putStr ""
renderBulletCountM (Just ship) = do
  ANSI.setCursorPosition 1 5
  putStr
    $ (++) "Bullts: " (show $ length (bullets ship))


renderLifeCountM :: Maybe Ship -> Dimension -> IO ()
renderLifeCountM Nothing _ = putStr ""
renderLifeCountM (Just ship@(Ship {lifes=(Just countLifes)})) terminalWindowsDimension = do
  ANSI.setCursorPosition 1 ((subtract 15) (width terminalWindowsDimension))
  putStr $ "Lifies: " ++ (show $ countLifes)


renderEnemyBulletCount :: [Maybe Ship] -> IO ()
renderEnemyBulletCount enemyListM = do
  ANSI.setCursorPosition 56 5
  putStr $ "Enemies Bullets:  " ++ (
    show $ foldr (
        \enUnit acc -> case enUnit of
                         (Just enemy )       -> (acc + length (bullets enemy))
                         (Nothing)           -> acc
        ) 0 enemyListM)

-- | runAndCleanBullet - function
runAndCleanBullet :: (Int -> Int) -> (Int -> Bool) -> Ship -> Maybe Ship
runAndCleanBullet func fpredicate ship@(Ship {bullets=[]}) = Just ship
runAndCleanBullet func fpredicate ship@(Ship {bullets=bulletsList}) = Just ship { bullets=( foldr cleanAndMove [] bulletsList)}
  where
    cleanAndMove (Bullet (Point nx ny)) newBulletsList
      | (fpredicate ny) = (Bullet {getBullet=(Point {x=nx, y=(func ny)})}) : newBulletsList
      | otherwise = (newBulletsList)
