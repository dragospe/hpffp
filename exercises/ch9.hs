eftBool :: Bool -> Bool -> [Bool]
eftBool x y = case (x, y) of
  (False, False) -> [False]
  (False, True)  -> [False, True]
  (True, False)  -> []
  (True, True)   -> [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y 
  | x == y = [x]
  | x > y = []
  | (x,y) == (LT, EQ) = [LT, EQ]
  | (x,y) == (EQ, GT) = [EQ, GT]
  | (x,y) == (LT, GT) = [LT, EQ, GT]
  | otherwise = undefined

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x > y = []
  | otherwise = reverse $ go x y x [x]
  where go :: Int -> Int -> Int -> [Int] -> [Int] 
        go x' y' counter list
          | counter < y = go x' y' (succ counter ) ((succ counter) : list)
          | otherwise = list
    
eftChar :: Char -> Char -> [Char]
eftChar x y
  | x > y = []
  | otherwise = reverse $ go x y x [x]
  where go :: Char -> Char -> Char -> [Char] -> [Char] 
        go x' y' counter list
          | counter < y = go x' y' (succ counter ) ((succ counter) : list)
          | otherwise = list

myWords :: [Char] -> [[Char]]
myWords w = dropWhile (=="") $ go w [] -- w for words
  where go :: [Char] -> [[Char]] -> [[Char]]
        go w' wl -- w' for words, wl for running word list
         | w' == [] = wl
         | otherwise = go (dropWhile (==' ') $ dropWhile (/=' ') w')
                          (wl ++ [takeWhile (/=' ') w'])

rmArticle :: [Char] -> [[Char]]
rmArticle sentence = filter isArticle $ words sentence
  where isArticle :: [Char] -> Bool
        isArticle x = not $ x  `elem` ["the", "a", "an"]

myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip2 :: [a] -> [b] -> [(a,b)]
myZip2 a b = myZipWith (\x y -> (x,y)) a b
