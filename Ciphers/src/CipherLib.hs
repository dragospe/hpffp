module CipherLib where

import           Data.Char

-- First we'll write a function that will calculate the offset of a character
-- from a starting character. This will allow us to determine the proper
-- "ordering" of a subset of the characters
charOffset ::  Char -> Char -> Int
charOffset c charFrom = ord c - ord charFrom

-- This gives us the offset of a (capital) letter
alphaToOffset :: Char -> Int
alphaToOffset c = charOffset c 'A'

-- Convert a offset to a letter, modulo 26
offsetToAlpha :: Int -> Char
offsetToAlpha n = chr $ n `mod` 26 + ord 'A'

-- Shift a single (upper case) character
ceaserShift :: Char -> Int -> Char
ceaserShift c n
  | isAlpha c = offsetToAlpha . (+n) $ alphaToOffset $ toUpper c
  | otherwise = c

-- Encode a string with the ceaser cipher
ceaserCipher :: Int -> [Char] -> [Char]
ceaserCipher _ ""     = ""
ceaserCipher n (c:cs) = ceaserShift c n : ceaserCipher n cs

unCeaser :: Int -> [Char] -> [Char]
unCeaser n s = ceaserCipher ((-1) * n) s


-- Vigenere cipher (ch 11): use a fixed keyword to determine the shifting
vigenereCipher :: [Char] -> [Char] -> [Char]
vigenereCipher kw pt = -- kw = "keyword", pt = "plaintext"
  go kw 0 pt "" -- keyword, keywordIndex, plaintext, ciphertext
  where go :: [Char] -> Int -> [Char] -> [Char] -> [Char]
        go _ _ "" ct = ct
        go kw kwIndex (ptHead: ptTail) ct
          | isAlpha ptHead = go kw
                             (mod (kwIndex + 1) (length kw)) -- Increase the index for alphabetic characters
                             ptTail
                             (ct ++ [ceaserShift ptHead $ alphaToOffset $ kw !! kwIndex])
          | otherwise = go kw
                           kwIndex -- keep the index for non-alphabetic characters
                           ptTail
                           (ct ++ [ptHead])

vigenereTest :: IO ()
vigenereTest = if vigenereCipher "ALLY" "MEET AT DAWN" == "MPPR AE OYWY"
              then putStrLn "Vigenere test passed"
              else putStrLn "Vigenere test failed"

-- Given a vignere keyword, find the word that decrypts the cipher
invertVigenereWord :: [Char] -> [Char]
invertVigenereWord [] = ""
invertVigenereWord (x:xs) = [offsetToAlpha $ (-1) * (alphaToOffset x) `mod` 26] ++ invertVigenereWord xs

unVigenere :: [Char] -> [Char] -> [Char]
unVigenere kw ct = vigenereCipher (invertVigenereWord kw) ct




