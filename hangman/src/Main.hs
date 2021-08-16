module Main where

import Control.Monad (forever)
import Data.Char (toLower, isAlpha)
import Data.Maybe (isJust)
import Data.List (intersperse, nub)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter (all isAlpha) $
                     filter gameLength aw)

  where gameLength w =
         let l = length (w :: String)
         in    l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]
---- Data constructors as follows:
-- String -> Words we're trying to guess
-- [Maybe Char] -> Characters successfully guessed so far
-- [Char] -> All characters guessed

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) 
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (take (length s) (repeat Nothing)) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = c `elem` w 

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = c `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar = 
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guess that character,\
                \ pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word\
                \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

numberIncorrect :: Puzzle -> Int
numberIncorrect p@(Puzzle _ filledInSoFar guessed)
  = length guessed - numberCorrect p

numberCorrect :: Puzzle -> Int
numberCorrect (Puzzle _ filledInSoFar _)
 = length (nub $ filter isJust filledInSoFar)

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle wordToGuess filledInSoFar guessed) = 
  if numberIncorrect p > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess 
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame p@(Puzzle _ _ guessed) = forever $ do
  putStrLn $ 
    "Current puzzle is: " ++ show p
  putStrLn $ "You have " 
    ++ (show $ 8 - numberIncorrect p)
    ++ " guesses left."
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> do 
      newPuzzle <- handleGuess p c 
      gameWin newPuzzle
      gameOver newPuzzle
      runGame newPuzzle
    _ -> putStrLn "Your guess must be a single character"


main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
