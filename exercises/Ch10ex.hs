module Ch10ex where

-- fibs = takeWhile (\x -> x < 100) $ take 20 $ 1 : scanl (+) 1 fibs
-- fibsN x = fibs !! x

-- myFactorial =  scanl (*) 1 [1..]

-- Words

--stops = "pbtdkg"
-- vowels = "aeiou"


myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = 
  if f x == True
  then True
  else myAny f xs

-- myAny3 :: (a -> Bool) -> [a] -> Bool
-- myAny3  = foldr (||) False . map 

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 = curry $ foldr (||) False . uncurry map 



foldr' :: Foldable t => ((a,b) -> b) -> b -> t a -> b
foldr' f acc l = foldr (curry f) acc l

myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f  = foldr' (uncurry ((.) (||) f)) False 

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 a = foldr ((||) . (==a)) False 


myElem2 :: Eq a => a -> [a] -> Bool
myElem2 = any . (==) 

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []  

myFilter :: ( a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a == True then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = (foldr (++) [] .)  . map

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> x) 

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Empty List" 
myMaximumBy f (x:xs) = (foldl 
  (\a b -> if f a b == GT then a else b)) x (x:xs)
