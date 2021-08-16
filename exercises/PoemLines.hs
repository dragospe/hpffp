module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Cound frame thy fearful\
              \ symmetry"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> Char -> [String]
myLines lines c = split lines c

shouldEqual = ["Tyger Tyger, burning bright",
               "In the forests of the night",
               "What immortal hand or eye",
               "Cound frame thy fearful symmetry"]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences '\n' == shouldEqual)

split :: String -> Char -> [String]
split str char = dropWhile (==[char]) $ go str char []
  where go :: String -> Char -> [String] -> [String]
        go s c wl
          | s == "" = wl
          | otherwise = go (dropWhile (==c) (dropWhile (/=c) s)) 
                           char
                           (wl ++ [takeWhile (/=c) s])
