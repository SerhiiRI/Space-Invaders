module Library.Ships
  ( Bullet(Bullet)
  , Ship(..)
  , defaultUserShip
  , defaultBulletView
  , defaultUserSprite
  , defaultShip
  , renderShipM
  , addBulletStack
  , addBulletOnEnemyStack
  , renderBulletCountM
  , renderEnemyBullets
  , renderGamerBullets
  , renderLifeCountM
  , clearBetweenEnemyShips
  , clearEnemyShips
  , runAndCleanBullet
  , getShipsLimitersPoint
  , moveEnemyShips
  , createEnemyShipTemplate
  , renderAllEnemyShips
  , intersectEnemyShips
  -- to deprecate
  , killing
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
defaultShip             = [" /^\\ ", " |^| ",  " \\^/ "]
defaultEnemySprite      :: (Width, Height)
defaultEnemySprite      = (length(head defaultShip), length(defaultShip))
defaultEnemyBulletView  :: [String]
defaultEnemyBulletView  = ["+", " "]

type Width = Int
type Height = Int
type Offset = Int
newtype Bullet = Bullet { getBullet :: Point } deriving (Show)




-- | Ship - main controll user ship
-- This ships may be abble to interraction
data Ship = Ship { point :: Point
                 , viewShip        :: [String]
                 , shipSprite      :: (Width, Height)
                 , viewBullet      :: [String]
                 , bullets         :: [Bullet]
                 , lifes           :: Int
                 } deriving (Show)

instance Eq Ship where
  (Ship {lifes=l1}) == (Ship {lifes=l2}) = l1 == l2
  (Ship {lifes=l1}) /= (Ship {lifes=l2}) = l1 /= l2


-- Enemy Ships functions
-- TODO: create offset to dimension enemymatrix
createEnemyShipTemplate :: Offset -> Dimension -> [[Maybe Ship]]
createEnemyShipTemplate offsetByY dimension = do
  let enemyMatrix = replicate 3 $ replicate 10 (Ship {
                                 point          = Point 0 0
                                 , viewShip     = defaultShip
                                 , shipSprite   = defaultEnemySprite
                                 , viewBullet   = defaultEnemyBulletView
                                 , bullets      = []
                                 , lifes        = 1
                                 })
  let (startX, startY) = generateStartPoint dimension
  updateMatrix startX startY enemyMatrix
  where
    updateMatrix :: Int -> Int -> [[Ship]] -> [[Maybe Ship]]
    updateMatrix px py enemyShips = zipWith (\enemies@(enm:enms) y -> toOffset (py+((+) 1 $ snd (shipSprite enm))*y) px enemies) enemyShips [1..]
    generateStartPoint :: Dimension -> (Int, Int)
    generateStartPoint (Dimension {height=xheight, width=xwidth}) = ((xwidth `div` 5), offsetByY+(xheight `div` 12))
    toOffset :: Int -> Int -> [Ship] -> [Maybe Ship]
    toOffset y_offset x_offset [] = []
    toOffset y_offset x_offset (e:ex) = [(Just (e {point=(Point x_offset y_offset)}))]
                                        ++ (toOffset y_offset ((fst (shipSprite e))+ x_offset) ex)


-- | addBulletOnEnemyStack make choise using Bool value about logic of shooting
-- StdGen - generator need to choise one of ships
addBulletOnEnemyStack :: Bool -> StdGen -> [Maybe Ship] -> [Maybe Ship]
addBulletOnEnemyStack False rGen enemyMatrix = enemyMatrix
addBulletOnEnemyStack True rGen enemyMatrix = do
  let (rv, _)  = randomR (0, (length enemyMatrix)-1) rGen
  let (tright, (x:xs)) = splitAt rv enemyMatrix in (tright ++ [ x >>= addBulletStack ] ++ xs)



-- USER FUNCTION. Create killled function for enemies.
-- create separate function for testing function by enemies
killing :: [Maybe Ship] -> Maybe Ship -> (Maybe Ship, [Maybe Ship], [Maybe Ship])
killing enemiesListship Nothing = (Nothing, enemiesListship, [])
killing enemiesListship (Just ship) = do
  let (nnewGamer, newEnemyList, _) = (foldl scaningForEnemies (ship, [], []) enemiesListship)
  if | (lifes nnewGamer) <= 0 -> (Nothing, newEnemyList, [])
     | otherwise -> let (newGamer, nnewEnemyList, listToClean) = (foldl scaningForUser (nnewGamer, [], []) newEnemyList) in (Just newGamer, nnewEnemyList, listToClean)
  where
    scaningForUser (currentShip, newEnemyList, toClean) currentEnemyShip =
      case currentEnemyShip of
          (Nothing) -> (currentShip, newEnemyList, toClean)
          (Just enemyShip) -> let (nGamerShip, nEnemyShip) =  intersectBulletSprite (+2) currentShip enemyShip [] in
            if | lifes nEnemyShip <= 0 -> (nGamerShip, newEnemyList, (Just nEnemyShip) : toClean)
               | otherwise -> (nGamerShip, (Just nEnemyShip) : newEnemyList, toClean)
    scaningForEnemies (currentShip, newEnemyList, toClean) currentEnemyShip =
      case currentEnemyShip of
        (Nothing)       -> (currentShip, newEnemyList, toClean)
        (Just enemyShip)-> let (newEnemy, newGamer) = intersectBulletSprite (subtract 1) enemyShip currentShip []
                           in (newGamer, (Just newEnemy) : newEnemyList, [])


intersectBulletSprite :: (Int -> Int) -> Ship -> Ship -> [Bullet] -> (Ship, Ship)
intersectBulletSprite yOffset enemy@(Ship {bullets=[]}) gamer newBulletsList = (enemy {bullets=newBulletsList}, gamer)
intersectBulletSprite yOffset enemy@(Ship {bullets=(blt:bltsls)}) ship currentBulletList = do
  if (inSection (point ship) (shipSprite ship) (getBullet blt))
    then intersectBulletSprite yOffset (enemy {bullets=bltsls}) (ship {lifes=((lifes ship)-1)}) currentBulletList
    else intersectBulletSprite yOffset (enemy {bullets=bltsls}) ship (blt : currentBulletList)
  where
    inSection :: Point -> (Int, Int) -> Point -> Bool
    inSection (Point shipX shipY) (shipWidth, shipHeight) (Point bulletX bulletY) =
      and ([
              and [shipY <= yOffset bulletY, (shipY+shipHeight) > yOffset bulletY],
              and [shipX < bulletX, (shipX+shipWidth) > bulletX]
           ])


intersectEnemyShips :: [Maybe Ship] -> Ship -> Maybe Ship
intersectEnemyShips [] gamerShip = Just gamerShip
intersectEnemyShips enemiesList gamerShip =
  if (inSectionFromAll enemiesList gamerShip False)
  then Nothing
  else Just gamerShip
  where
    inSectionFromAll [] gamer finishBool = finishBool
    inSectionFromAll ((Just enemy):enemiesls) gamer finishBool = if (inSection (point gamer) (shipSprite enemy) (point enemy))
      then inSectionFromAll [] gamer True
      else inSectionFromAll enemiesls gamer False
    inSection :: Point -> (Int, Int) -> Point -> Bool
    inSection (Point _ shipY) (_, shipHeight) (Point _ enemyY) = shipY == (shipHeight)+enemyY



moveEnemyShips :: (Int -> Int) -> (Int -> Int) ->  Bool -> [Maybe Ship] -> [Maybe Ship]
moveEnemyShips _ _ False ship = ship
moveEnemyShips moveLRFunc moveDownFunc True shipsList = map (
  \maybeShip -> case maybeShip of
                  (Nothing) -> Nothing
                  (Just ship@(Ship {point=(Point xp yp)}))
                    -> Just ship {point=(Point (moveLRFunc xp) (moveDownFunc yp))}) shipsList

getShipsLimitersPoint :: [Maybe Ship] -> (Int, Int)
getShipsLimitersPoint [] = (0, 0)
getShipsLimitersPoint enemyList@((Just enemy):_) =
  let listOfX = foldr (\x acc -> case x of
                          (Nothing)                                                     -> acc
                          (Just ship@(Ship {point=(Point xposition yposition)}))        -> xposition : acc
                      )  [] enemyList
  in (minimum listOfX, (maximum listOfX + (fst $ shipSprite enemy)))


clearBetweenEnemyShips :: [Maybe Ship] -> IO ()
clearBetweenEnemyShips [] = putStr ""
clearBetweenEnemyShips ((Nothing:enems)) = clearBetweenEnemyShips enems
clearBetweenEnemyShips ((Just ship@(Ship { point=(Point xp yp) })):enems) = do
  ANSI.setCursorPosition (yp - 1) 0
  ANSI.clearLine
  clearBetweenEnemyShips enems

clearEnemyShips :: [Maybe Ship] -> IO ()
clearEnemyShips [] = putStr ""
clearEnemyShips enemyMatrix = do
  mapM_ cleanShipM enemyMatrix

cleanShipM :: Maybe Ship -> IO ()
cleanShipM (Nothing) = putStr ""
cleanShipM (Just (Ship {point=(Point x_position y_position), viewShip=[]})) = ANSI.setCursorPosition y_position x_position
cleanShipM (Just ship@(Ship { point=(Point x_position y_position), viewShip=(l:ls) })) = do
  ANSI.setCursorPosition (y_position) x_position
  putStr $ concat $ replicate (length l) " "
  cleanShipM (Just (ship {point=(Point x_position (y_position+1)), viewShip=ls}))

addBulletStack :: Ship -> Maybe Ship
addBulletStack ship@(Ship {point=(Point xloc yloc), shipSprite=(spriteW,spriteH), bullets=bulletList }) =
  if ((length bulletList) > 3)
  then (Just ship)
  else Just (ship {bullets=(newBulletPoint : bulletList)})
  where
    newBulletPoint = Bullet (Point ((div spriteW 2) + xloc) (yloc))

renderShipM :: ANSI.Color -> Maybe Ship -> IO ()
renderShipM shipColor (Nothing) = putStr ""
renderShipM shipColor (Just (Ship {point=(Point x_position y_position), viewShip=[]})) = ANSI.setCursorPosition y_position x_position
renderShipM shipColor (Just ship@(Ship { point=(Point x_position y_position), viewShip=(l:ls) })) = do
  ANSI.setCursorPosition y_position x_position
  colorize l shipColor
  renderShipM shipColor (Just (ship {point=(Point x_position (y_position+1)), viewShip=ls}))

renderAnimationShipM :: ANSI.Color -> Int -> Maybe Ship -> IO ()
renderAnimationShipM _ animationStage (Nothing) = putStr ""
renderAnimationShipM shipColor animationStage  (Just (Ship {point=(Point x_position y_position), viewShip=[]})) = ANSI.setCursorPosition y_position x_position
renderAnimationShipM shipColor animationStage (Just ship@(Ship { point=(Point x_position y_position), viewShip=view })) = do
  ANSI.setCursorPosition y_position x_position
  colorize (view !! animationStage)  shipColor

renderAllEnemyShips :: ANSI.Color -> Int -> [Maybe Ship] -> IO ()
renderAllEnemyShips color animationStage [] = putStrLn ""
renderAllEnemyShips color animationStage enemyMatrix = do
  mapM_ (renderAnimationShipM color animationStage) enemyMatrix

renderBulletsMechanism :: (Int -> Bool) -> (Int -> Int) -> Maybe Ship -> IO ()
renderBulletsMechanism _ _ Nothing = putStr ""
renderBulletsMechanism _ _ (Just ship@(Ship {bullets=[]})) = putStr ""
renderBulletsMechanism borderPredicate incFunc (Just ship@(Ship {viewBullet=vb, bullets=blts@(b:bx)})) = do
  rBullet vb b
  renderBulletsMechanism borderPredicate incFunc (Just (ship {bullets=bx}))
  where rBullet (vv:vvb) bulletPoint@(Bullet {getBullet=(Point x_position y_position)}) = do
          ANSI.setCursorPosition y_position x_position
          if (borderPredicate y_position)
            then putStr " "
            else putStr vv
          if (null vvb)
            then ANSI.setCursorPosition y_position x_position
            else rBullet vvb (Bullet (Point {x=x_position, y=(incFunc y_position)}))

-- | Bullets render function for Gamer
-- send to function renderBulletMechanism border predicate
-- ,moving bullet step, and gamer ship controller
renderGamerBullets :: Maybe Ship -> IO ()
renderGamerBullets ships = renderBulletsMechanism (< 2) (+1) ships

renderEnemyBullets :: [Maybe Ship] -> Dimension -> IO ()
renderEnemyBullets [] _ = putStr ""
renderEnemyBullets enemyMatrix dimension = mapM_ (renderBulletsMechanism ( > (height dimension -1)) (subtract 1)) enemyMatrix

renderBulletCountM :: Maybe Ship -> IO ()
renderBulletCountM Nothing = putStr ""
renderBulletCountM (Just ship) = do
  ANSI.setCursorPosition 1 5
  putStr $ (++) "Bullts: " (show $ length (bullets ship))

renderLifeCountM :: Maybe Ship -> Dimension -> IO ()
renderLifeCountM Nothing _ = putStr ""
renderLifeCountM (Just ship@(Ship {lifes=countLifes})) terminalWindowsDimension = do
  ANSI.setCursorPosition 1 ((subtract 15) (width terminalWindowsDimension))
  putStr $ "Lifies: " ++ (show $ countLifes)

colorize :: String -> Color -> IO ()
colorize text color = do
  setSGR [SetColor Foreground Vivid color]
  --setSGR [SetColor Background Vivid Blue]
  putStr text
  setSGR [Reset]

-- | runAndCleanBullet - function
runAndCleanBullet :: Int -> (Int -> Int) -> (Int -> Bool) -> Ship -> Maybe Ship
runAndCleanBullet f func fpredicate ship@(Ship {bullets=[]}) = Just ship
runAndCleanBullet f func fpredicate ship@(Ship {bullets=bulletsList}) = Just ship { bullets=( foldr cleanAndMove [] bulletsList)}
  where
    cleanAndMove (Bullet (Point nx ny)) newBulletsList
      | (f /= 0) = (Bullet {getBullet=(Point {x=nx, y=(ny)})}) : newBulletsList
      | (fpredicate ny) = (Bullet {getBullet=(Point {x=nx, y=(func ny)})}) : newBulletsList
      | otherwise = (newBulletsList)
