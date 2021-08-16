module Ch11Ex where
import Data.Char as Char

-- As patterns


-- Subsequence membership
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf _ [] = False 
isSubseqOf [] _ = True
isSubseqOf xf@(x:xs) (s:ss) 
  | x == s = isSubseqOf xs ss
  | otherwise = isSubseqOf xf ss 
  

-- Split words, add capitalized as tuple
capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords s = [(y, toUpper x : xs) | y@(x:xs) <- words s]


capitalizedWord :: String -> String
capitalizedWord [] = []
capitalizedWord (x:xs) = toUpper x : xs


-- Capitalize all sentence starts in a paragraph.
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
-- Basic strategy here is to find dots and add all characters to a return string if they're not alphabetic.
-- If they are alphabetic, capitalize the first, and add the rest of the string until a dot occurs. 
capitalizeParagraph p = go p ""
  where go :: String -> String -> String
        go [] ret = ret
        go orig@(x:xs) ret 
          -- If x is a dot, then add everything up to the next letter to the return string
          | x == '.' = go 
                        (dropWhile (not . isAlpha) orig)
                        (ret ++ takeWhile (not . isAlpha) orig)
          -- Otherwise, its NOT a dot. So we must have an alphabetic character. Capitalize it, and move on
          | otherwise = go 
                        (dropWhile (/='.') orig)
                        (ret ++ toUpper x : takeWhile (/='.') xs)

