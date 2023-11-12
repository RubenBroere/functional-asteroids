{-# LANGUAGE NamedFieldPuns #-}
module World (World, makeWorld, updateWorld, handleEvents, gameOver, currentScore) where

import System.Random hiding (split)
import Graphics.Gloss hiding (Vector)

import Player
import Types
import InputHandler
import Asteroid
import Bullet
import Ufo
import Graphics.Gloss.Interface.IO.Game hiding (Vector)
import Renderer
import Sprites
import Explosion

data World = World
    { pressedKeys :: PressedKeys
    , worldSize  :: (Int, Int)
    , stdGen      :: StdGen
    , score       :: Score
    , highScore   :: Score
    , player      :: Player
    , asteroids   :: [Asteroid]
    , ufos        :: [Ufo]
    , bullets     :: [Bullet]
    , elapsedTime :: Time
    , gameState   :: GameState
    , explosions  :: [Explosion]
    }

makeWorld :: (Int, Int) -> StdGen -> Score -> World
makeWorld screenSize gen score' = World
    { pressedKeys = makeKeys
    , worldSize = screenSize
    , stdGen = gen
    , score = 0
    , highScore = score'
    , player = makePlayer
    , asteroids = []
    , ufos = []
    , bullets = []
    , elapsedTime = 0
    , gameState = Paused
    , explosions = []
    }

gameOver :: World -> Bool
gameOver = (GameOver ==) . gameState

currentScore :: World -> Score
currentScore = score

-- Drawing

instance Drawable World where
    draw w = Pictures
        [ drawHud w
        , draw $ player w
        , drawAll $ asteroids w
        , drawAll $ bullets w
        , drawAll $ ufos w
        , drawAll $ explosions w]

drawHud :: World -> Picture
drawHud w = Pictures
    [ topRight screen $ drawLives lives
    , bottomLeft screen $ drawBanner $ show state
    , topLeft screen highScoreText
    , topLeft screen scoreText
    ]
    where
        screen = worldSize w
        lives = getLives $ player w
        state = gameState w
        highScoreText = Translate 10 (-30) $ Scale 0.2 0.2 $ Color white $ Text $ show $ highScore w
        scoreText = Translate 10 (-60) $ Scale 0.2 0.2 $ Color white $ Text $ show $ score w

drawLives :: Int -> Picture
drawLives lives = Color white $ Pictures [Translate (-15 - fromIntegral x*20) (-20) $ Scale 0.5 0.5 playerSprite | x <- [0..(lives-1)]]

drawBanner :: String -> Picture
drawBanner str = Translate 10 20 $ Color white $ Scale 0.5 0.5 $ Text str

-- Standard updating

updateWorld :: Time -> World -> World
updateWorld _ w@World{ gameState=Paused } = w
updateWorld _ w@World{ gameState=GameOver } = w
updateWorld dt world = updateSteps dt world
    [ applyInput
    , updatePlayer
    , updateAsteroids
    , updateBullets
    , updateElapsedTime
    , updateGameState
    , updateUfos
    , updateExplosions]

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
onKeyUp (SpecialKey KeySpace) w@World{ gameState=GameOver } = makeWorld (1280, 720) (stdGen w) (highScore w)
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
        newBullet = makeBullet True (position p) (velocity p) (rotation p)

handleSlowDown :: Time -> World -> World
handleSlowDown dt w@World{ player, pressedKeys }
    | not $ pressForward pressedKeys = w{ player=deceleratePlayer dt player }
    | otherwise = w

handlePlayerDeath :: World -> World
handlePlayerDeath w | playerIsCol w = w { player=handleDeath (player w) }
                    | otherwise = w

playerIsCol :: World -> Bool
playerIsCol w = isCollisions (player w) (asteroids w) ||
                isCollisions (player w) (bullets w) ||
                isCollisions (player w) (ufos w)

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
    { asteroids=updateAll dt $ filter (not . isGarbage (worldSize world)) $ asteroids world }

handleSpawntimer :: World -> World
handleSpawntimer w  | canSpawnMore (asteroids w) = spawnAsteroid w
                    | otherwise = w

spawnAsteroid :: World -> World
spawnAsteroid w@World{ asteroids } = w{ asteroids=asteroid:asteroids, stdGen=gen }
    where
        (asteroid, gen) = generateAsteroid (worldSize w) (stdGen w)

killHitAsteroids :: World -> World
killHitAsteroids w@World{ bullets, asteroids } = w
    { bullets=newBullets
    , asteroids=newAsteroids
    , stdGen=newGen
    , score=round points + score w
    , explosions=roidsPlosions ++ explosions w}
    where
        playerBullets = filter isPlayerBullet bullets
        newBullets = filter (\x -> not (isCollisions x asteroids) || not (isPlayerBullet x)) bullets
        newAsteroids = splitRoids ++ filter (\x -> not (isCollisions x playerBullets)) asteroids
        (splitRoids, newGen) = foldr acc ([], stdGen w) hitAsteroids
        hitAsteroids = filter (`isCollisions` playerBullets) asteroids
        roidsPlosions = map (\x -> makeExplosion (size x) (position x)) hitAsteroids
        points = foldr (\x y -> size x + y) 0 hitAsteroids

        acc :: Asteroid -> ([Asteroid], StdGen) -> ([Asteroid], StdGen)
        acc a0 (as, gen0) = maybe (as, gen0) (combine as) (split a0 gen0)

        combine :: [Asteroid] -> (Asteroid, Asteroid, StdGen) -> ([Asteroid], StdGen)
        combine as (a1, a2, gen) = (a1:a2:as, gen)

updateExplosions :: Time -> World -> World
updateExplosions dt w = w { explosions=updateAll dt $ explosions w }

-- Ufo

updateUfos :: Time -> World -> World
updateUfos dt = ufoShooting . killHitUfos . handleUfoSpawning . updateUfoPositions dt

handleUfoSpawning :: World -> World
handleUfoSpawning w
    | null (ufos w) && elapsedTime w > 10 = w{ ufos=ufo:ufos w, stdGen=gen }
    | length (ufos w) < 2 && elapsedTime w > 30 = w{ ufos=ufo:ufos w, stdGen=gen }
    | length (ufos w) < 2 && elapsedTime w > 50 = w{ ufos=ufoS:ufos w, stdGen=genS }
    | otherwise = w
    where
        (ufo, gen) = generateUfo (worldSize w) (stdGen w) Random
        (ufoS, genS) = generateUfo (worldSize w) (stdGen w) TargetPlayer

updateUfoPositions :: Time -> World -> World
updateUfoPositions dt w = w{ ufos=ufos', stdGen=rnd' }
    where
        (ufos', rnd') = moveToTargets (worldSize w) (stdGen w) $ updateAll dt $ ufos w

killHitUfos :: World -> World
killHitUfos w = w{ bullets=bullets', ufos=ufos', score=score w + points }
    where
        bullets' = filter (\x -> not $ isCollisions x $ ufos w) $ bullets w
        ufos' = filter (\x -> not $ isCollisions x $ bullets w) $ ufos w
        points = (length (ufos w) - length ufos') * 100

ufoShooting :: World -> World
ufoShooting w = w{ bullets=newBullets ++ bullets w, stdGen=rnd, ufos=ufos' }
    where
        (newBullets, rnd) = foldr acc ([], stdGen w) $ ufos w
        ufos' = map (\x -> (if ufoIsReloading x then x else ufoShoot x)) $ ufos w

        acc :: Ufo -> ([Bullet], StdGen) -> ([Bullet], StdGen)
        acc ufo (bullets', rnd')
            | ufoIsReloading ufo = (bullets', rnd')
            | otherwise = (bullet:bullets', rnd'')
            where
                (bullet, rnd'') = spawnUfoBullet (position $ player w) rnd' ufo

spawnUfoBullet :: Vector -> StdGen -> Ufo -> (Bullet, StdGen)
spawnUfoBullet (tx, ty) rnd ufo 
    | isSmart ufo = (makeBullet False (position ufo) (0, 0) smartTheta, rnd'')
    | otherwise = (makeBullet False (position ufo) (0, 0) theta, rnd')
    where
        (theta, rnd') = randomR (0, 2*pi) rnd
        (x, y) = position ufo
        angle = atan2 (tx - x) (ty - y)
        (smartTheta, rnd'') = randomR (angle - deviance, angle + deviance) rnd
        deviance = 0.2

-- Updating miscellaneous

updateElapsedTime :: Time -> World -> World
updateElapsedTime dt world@World{ elapsedTime } = world{ elapsedTime=elapsedTime + dt }

updateBullets :: Float -> World -> World
updateBullets dt w@World{ bullets } = w{ bullets=filter (not . isExpired) $ updateAll dt bullets }


updateGameState :: Float -> World -> World
updateGameState _ w@World{ player }
    | getLives player == 0 = w { gameState=GameOver, highScore=max (highScore w) (score w) }
    | otherwise = w

