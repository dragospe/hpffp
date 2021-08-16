module Main where

import CipherLib
import Data.Char (toLower, isAlpha)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import System.IO

data CipherSelection = Ceaser | Vigenere deriving (Eq, Show)

chooseCipher :: Char -> Maybe CipherSelection
chooseCipher c 
  | toLower c == 'c' = Just Ceaser
  | toLower c == 'v' = Just Vigenere
  | otherwise = Nothing
   
chooseCipher' :: IO CipherSelection
chooseCipher' = do
  putStrLn "Choose a cipher:"
  putStrLn "[c] - Ceaser  | [v] - Vignere" 
  cipherChoice <- getLine
  case cipherChoice of
    [c] -> do
      if isJust $ chooseCipher c
      then return $ (\(Just a) -> a) (chooseCipher c)
      else do
        putStrLn "Invalid selection. Try again."
        chooseCipher'
    _ -> do
          putStrLn "Invalid selection. Try again."
          chooseCipher'

data EncodeSelection = Encode | Decode deriving (Eq, Show)

encodeOrDecode :: Char -> Maybe EncodeSelection
encodeOrDecode c
  | toLower c == 'e' = Just Encode
  | toLower c == 'd' = Just Decode
  | otherwise = Nothing

encodeOrDecode' :: IO EncodeSelection
encodeOrDecode' = do
  putStrLn "Do you want to encode or decode?"
  putStrLn "[e] - Encode  | [d] - Decode" 
  encodeChoice <- getLine
  case encodeChoice of
    [c] -> if isJust $ encodeOrDecode c
           then return $ (\(Just a) -> a) (encodeOrDecode c)
           else do
            putStrLn "Invalid selection. Try again"
            encodeOrDecode'
    _ -> do
          putStrLn "Invalid selection. Try again."
          encodeOrDecode'

getKeyword :: IO String
getKeyword = do
  putStr "Enter a keyword: "
  keyword <- getLine
  isKeyword keyword
  where isKeyword w = 
          if all isAlpha w
          then return w
          else do
            putStrLn "Keywords must be alphabetic only. Try again"
            getKeyword
    
getShift :: IO Int
getShift = do
  putStr "Enter a shift: "
  shift <- getLine
  if isJust (readMaybe shift :: Maybe Int) 
  then return $ (\(Just a) -> a) (readMaybe shift :: Maybe Int)
  else do
    putStrLn "Shifts must be integers. Try again."
    getShift

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  encodeChoice <- encodeOrDecode'
  cipherChoice <- chooseCipher'
  putStr "Enter the text to encode or decode: "
  text <- getLine
  case cipherChoice of
    Ceaser -> do
                shift <- getShift
                if encodeChoice == Encode
                then putStrLn $ ceaserCipher shift text
                else putStrLn $ unCeaser shift text
    Vigenere -> do 
                keyword <- getKeyword
                if encodeChoice == Encode
                then putStrLn $ vigenereCipher keyword text
                else putStrLn $ unVigenere keyword text
