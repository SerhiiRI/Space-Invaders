module Library.Ships
  ( Bullet(Bullet)
  , GamerShip(..)
  --, EnemyShip(EnemyShip,EmptyShip)
  , defaultUserShip
  , defaultBulletView
  , defaultUserSprite
  , renderShip'
  --, renderShip''
  ) where

-- Point (X, Y)
import Library.Vector
import System.Console.ANSI as ANSI

defaultUserShip :: [String]
defaultUserShip = [ "     /#\\     ", "  \\ / | \\ /  ", "  /\\\\ | //\\  " ,"     - -     " ]

defaultUserSprite :: (Width, Height)
defaultUserSprite = (length(defaultUserShip), length(head defaultUserShip))

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




renderShip' :: Maybe GamerShip -> IO ()
renderShip' (Nothing) = do
  putStrLn ""
renderShip' (Just (GamerShip {
                      point=(Point x_position y_position)
                      , viewShip=(l:ls)
                      , shipSprite=spr
                      , viewBullet=bul
                      , bullets=bulle
                      , lifes=lif
                      })) = do
  ANSI.setCursorPosition y_position x_position
  putStrLn l
  renderShip' (Just (GamerShip (Point x_position (y_position+1)) ls spr bul bulle lif))
renderShip' (Just (GamerShip {point=(Point x_position y_position), viewShip=[]})) = do
  ANSI.setCursorPosition y_position x_position


{-
renderShip'' :: Maybe GamerShip -> IO ()
renderShip'' game
        | game == (Nothing) = putStrLn ""
        | game == (Just (GamerShip {point=(Point x_position y_position), viewShip=(l:ls)})) = do
            ANSI.setCursorPosition y_position x_position
            putStrLn l
            renderShip'' (game {viewShip=ls})
        | game == (Just (GamerShip {point=(Point x_position y_position), viewShip=[]})) = do
            ANSI.setCursorPosition y_position x_position
-}


