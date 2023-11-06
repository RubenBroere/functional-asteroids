{-# LANGUAGE NamedFieldPuns #-}
module World (World, makeWorld, updateWorld, handleEvents) where

import System.Random hiding (split)
import Graphics.Gloss

import Player
import Types
import InputHandler
import Asteroid
import Bullet
import Ufo
import Graphics.Gloss.Interface.IO.Game
import Renderer
import Sprites

data World = World
    { pressedKeys :: PressedKeys
    , worldSize  :: (Int, Int)
    , stdGen      :: StdGen
    , score       :: Score
    , player      :: Player
    , asteroids   :: [Asteroid]
    , ufos        :: [Ufo]
    , bullets     :: [Bullet]
    , elapsedTime :: Time
    , gameState   :: GameState
    }

makeWorld :: (Int, Int) -> World
makeWorld screenSize = World
    { pressedKeys = makeKeys
    , worldSize = screenSize
    , stdGen = mkStdGen 42
    , score = 0
    , player = makePlayer
    , asteroids = []
    , ufos = []
    , bullets = []
    , elapsedTime = 0
    , gameState = Paused
    }

-- Drawing

instance Drawable World where
    draw w = Pictures
        [ drawHud w
        , draw $ player w, Pictures $ map draw $ asteroids w
        , Pictures $ map draw $ bullets w]

drawHud :: World -> Picture
drawHud w = Pictures [topLeft screen $ drawLives lives, bottomLeft screen $ drawBanner $ show state]
    where
        screen = worldSize w
        lives = getLives $ player w
        state = gameState w

drawLives :: Int -> Picture
drawLives lives = Color white $ Pictures [Translate (15 + fromIntegral x*20) (-20) $ Scale 0.5 0.5 playerSprite | x <- [0..(lives-1)]]

drawBanner :: String -> Picture
drawBanner str = Translate 10 20 $ Color white $ Scale 0.5 0.5 $ Text str

-- Standard updating

updateWorld :: Time -> World -> World
updateWorld _ w@World{ gameState=Paused } = w
updateWorld _ w@World{ gameState=GameOver } = w
updateWorld dt world = updateSteps dt world [applyInput, updatePlayer, updateAsteroids, updateBullets, updateElapsedTime, updateGameState]

updateSteps :: Time -> World -> [Time -> World -> World] -> World
updateSteps dt = foldr (\component w -> component dt w)

-- Key up events

handleEvents :: Event -> World -> World
handleEvents (EventResize screenSize) w = w{ worldSize=screenSize }
handleEvents event w@World{ pressedKeys } = handleKeyUps event $ w{ pressedKeys=handleInput event pressedKeys }

handleKeyUps :: Event -> World -> World
handleKeyUps (EventKey k Up _ _) w = onKeyUp k w
handleKeyUps _ w = w

onKeyUp :: Key -> World -> World
onKeyUp (SpecialKey KeySpace) w@World{ gameState=Paused } = w{ gameState=Game }
onKeyUp (SpecialKey KeySpace) World{ gameState=GameOver } = makeWorld (1280, 720)
onKeyUp (Char 'p') w = w{ gameState=Paused }
onKeyUp _ w = w

-- Player

updatePlayer :: Time -> World -> World
updatePlayer dt w = handlePlayerDeath . handleSlowDown dt $ w{ player=handleBounds (worldSize w) $ update dt (player w) }

applyToPlayer :: (Time -> Player -> Player) -> Time -> World -> World
applyToPlayer f dt w = w{ player=f dt $ player w }

playerShoot :: Time -> World -> World
playerShoot _ w
    | reloading p = w
    | otherwise = w{ bullets=newBullet:bullets w, player=shoot p }
    where
        p = player w
        newBullet = makeBullet (position p) (velocity p) (rotation p)

handleSlowDown :: Time -> World -> World
handleSlowDown dt w@World{ player, pressedKeys }
    | not $ pressForward pressedKeys = w{ player=deceleratePlayer dt player }
    | otherwise = w

handlePlayerDeath :: World -> World
handlePlayerDeath w | playerIsCol w = w { player=handleDeath (player w) }
                    | otherwise = w

playerIsCol :: World -> Bool
playerIsCol w = any (isCollision $ player w) $ asteroids w

-- Input Handling

applyInput :: Time -> World -> World
applyInput dt w@World{ pressedKeys } = foldr applyIfPressed w inputActions
    where
        applyIfPressed :: (PressedKeys -> Bool, Time -> World -> World) -> World -> World
        applyIfPressed (isPressed, action) accData
            | isPressed pressedKeys = action dt accData
            | otherwise = accData

inputActions :: [(PressedKeys -> Bool, Time -> World -> World)]
inputActions = [
    (pressForward, applyToPlayer acceleratePlayer),
    (pressLeft, applyToPlayer $ rotatePlayer (-pi)),
    (pressRight, applyToPlayer $ rotatePlayer pi),
    (pressShoot, playerShoot)
    ]

-- Asteroid

updateAsteroids :: Time -> World -> World
updateAsteroids dt world = killHitAsteroids . handleSpawntimer $ world
    { asteroids=map (update dt) $ filter (not . isGarbage (worldSize world)) $ asteroids world }

handleSpawntimer :: World -> World
handleSpawntimer w  | canSpawnMore (asteroids w) = spawnAsteroid w
                    | otherwise = w

spawnAsteroid :: World -> World
spawnAsteroid w@World{ asteroids } = w{ asteroids=asteroid:asteroids, stdGen=gen }
    where
        (asteroid, gen) = generateAsteroid (worldSize w) (stdGen w)

killHitAsteroids :: World -> World
killHitAsteroids w@World{ bullets, asteroids } = w { bullets=newBullets, asteroids=newAsteroids, stdGen=newGen }
    where
        newBullets = filter (\x -> not (isCollisions x asteroids)) bullets
        newAsteroids = splitRoids ++ filter (\x -> not (isCollisions x bullets)) asteroids
        (splitRoids, newGen) = foldr acc ([], stdGen w) hitAsteroids
        hitAsteroids = filter (`isCollisions` bullets) asteroids

        acc :: Asteroid -> ([Asteroid], StdGen) -> ([Asteroid], StdGen)
        acc a0 (as, gen0) = maybe (as, gen0) (combine as) (split a0 gen0) 

        combine :: [Asteroid] -> (Asteroid, Asteroid, StdGen) -> ([Asteroid], StdGen)
        combine as (a1, a2, gen) = (a1:a2:as, gen)

updateElapsedTime :: Time -> World -> World
updateElapsedTime dt world@World{ elapsedTime } = world{ elapsedTime=elapsedTime + dt }

updateBullets :: Float -> World -> World
updateBullets dt w@World{ bullets } = w{ bullets=filter (not . isExpired) $ map (update dt) bullets }

updateGameState :: Float -> World -> World
updateGameState _ w@World{ player }
    | getLives player == 0 = w { gameState=GameOver }
    | otherwise = w

