module Ch13ex where

import Control.Monad (forever)
import Data.Char (isAlpha, toLower)
import System.Exit (exitSuccess)

isPalindrome :: String -> Bool
isPalindrome line = stripped == (reverse stripped)
  where stripped = map toLower $ filter isAlpha line

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do 
      putStrLn "Nope!"
      exitSuccess

--- gimmeperson
--
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = 
    NameEmpty 
  | AgeTooLow 
  | PersonInvalidUnknown String 
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age 
 | name /= "" && age > 0 = Right $ Person name age
 | name == "" = Left NameEmpty
 | not (age > 0) = Left AgeTooLow
 | otherwise = Left $ PersonInvalidUnknown $ 
                "Name was: " ++ show name ++
                " Age was: " ++ show age

-- gimmePerson :: IO ()
-- gimmePerson = do
  
