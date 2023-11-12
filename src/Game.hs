module Game ( run ) where

import Graphics.Gloss
import World
import Types
import System.Random (initStdGen)
import Control.Monad
import Graphics.Gloss.Interface.IO.Game
import System.IO
import Text.Read (readMaybe)
import Data.Maybe

run :: IO ()
run = do
    gen <- initStdGen
    highScore <- getHighScore

    playIO screen black 120 (makeWorld screenSize gen highScore) viewIO inputIO stepIO
    where
        screenSize = (1280, 720)
        screen = InWindow "Functional Asteroid" screenSize (0, 0)

getHighScore :: IO Score
getHighScore = do
    let scoreFile = "scores.txt"

    handle <- openFile scoreFile ReadWriteMode
    fileContents <- hGetContents' handle

    let scores = mapMaybe maybeInt (lines fileContents)
    if null scores then
        return 0
    else
        return $ maximum scores

    where
       maybeInt :: String -> Maybe Int
       maybeInt = readMaybe

viewIO :: World -> IO Picture
viewIO w = do return $ draw w

inputIO :: Event -> World -> IO World
inputIO e@(EventKey (SpecialKey KeySpace) Up _ _) w = do
    when (gameOver w) $ appendFile "scores.txt" $ show (currentScore w) ++ "\n"
    return $ handleEvents e w

inputIO e w = do return $ handleEvents e w

stepIO :: Time -> World -> IO World
stepIO dt w = do return $ updateWorld dt w
