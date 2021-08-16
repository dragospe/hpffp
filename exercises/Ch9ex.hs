module Ch9ex where

import Data.Bool
import Data.Char

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = if (length $ filter f l) > 0 then True else False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = if e == x then True else myElem e xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 e xs = myAny (==e) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- Flatten a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish ls = go ls []
  where go :: [[a]] -> [a] -> [a]
        go [] rList = rList
        go (l':ls') rList =  go ls' (l' ++ rList)

-- Flatten a list of lists into a list, mapping a function
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f ls = squish $ map f ls

-- Use "squishMap" to re-implement squish
squishAgain :: [[a]] -> [a]
squishAgain ls = squishMap (\x -> x) ls

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Empty List"
myMaximumBy f (x:xs) = go f x xs
  where go :: (a -> a -> Ordering) -> a -> [a] -> a
        go _ x [] = x
        go f x (y:ys) = go f
          (if f y x == GT then y else x)
          ys

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Empty List"
myMinimumBy f (x:xs) = go f x xs
  where go :: (a -> a -> Ordering) -> a -> [a] -> a
        go _ x [] = x
        go f x (y:ys) = go f
          (if f y x == LT then y else x)
          ys

myMaximum :: (Ord a) => [a] -> a
myMaximum l = myMaximumBy compare l

myMinimum :: (Ord a) => [a] -> a
myMinimum l = myMinimumBy compare l
