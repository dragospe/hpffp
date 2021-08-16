module R where

rvrs :: String -> String
rvrs s = z ++ y ++ x
  where 
    x = take 5 s
    y = drop 5 (take 9 s)
    z = drop 9 s

main :: IO ()
main = print $ rvrs "Curry is awesome"

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (reverse x) == x
