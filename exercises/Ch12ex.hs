module Ch12ex where
import Data.Bool
import Data.List (intercalate)

notThe :: String -> Maybe String
notThe s = bool (Just "the") Nothing (s /= "the")

replaceThe :: String -> String
replaceThe s = intercalate " " $ go (words s) []
  where go :: [String] -> [String] -> [String]
        go [] output = output
        go (x:xs) output = 
          if notThe x == Nothing
          then [x] ++ go xs output  
          else ["a"]  ++ go xs output

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go diwords 0
  where diwords = zip w $ tail w
          where w = words s
        go :: [(String,String)] -> Integer -> Integer
        go [] count = count
        go (dw:dws) count = go dws $ bool (count) (count + 1) (isTBV dw)
          where
            isTBV :: (String, String) -> Bool
            isTBV dw' = if fst dw' == "the" && isVowel (head (snd dw'))
                        then True
                        else False
              
-- Obviously some of the functions above could be broken out and reused, but I'm in a rush
-- Maybe I'll refactor later... but probably not
countVowels :: String -> Int
countVowels = length . filter isVowel

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word' -- Note that this, in particular, expects a single word in the string; no spaces, punctuation, etc.
mkWord s = if countVowels s > countConsonants s
           then Nothing
           else Just $ Word' s
  where countConsonants :: String -> Int
        countConsonants s' = length s' - countVowels s'



data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger n = go n 0
  where go :: Nat -> Integer -> Integer
        go Zero i = i
        go (Succ n') i = go n' (i+1)

integerToNat :: Integer -> Maybe Nat
integerToNat i = go i Zero
  where go :: Integer -> Nat -> Maybe Nat
        go int nat 
          | int < 0 = Nothing
          | int == 0 = Just nat
          | int > 0 = go (int - 1) (Succ nat)
          | otherwise = error "This shouldn't ever happen."

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id 

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes  = map (\(Just x) -> x) . filter isJust  


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe l 
  | False `elem` (map isJust l) = Nothing
  | otherwise = Just (map (\(Just x) -> x) l)

lefts' :: [Either a b] -> [a]
lefts' = catMaybes . (foldr (\x y-> f x : y) []) 
  where f :: Either a b -> Maybe a
        f (Left a) = Just a
        f (Right _) = Nothing

rights' :: [Either a b] -> [b]
rights' = catMaybes . (foldr (\x y -> f x : y) [])
  where f :: Either a b -> Maybe b
        f (Left _) = Nothing
        f (Right b) = Just b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([],[])
partitionEithers' l = (lefts' l, rights' l)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe2' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe2' f x = either' (\_ -> Nothing) (Just . f) x

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = if isJust (f b) 
                then justFst (f b) : myUnfoldr f (justSnd (f b))
                else []
  where justFst (Just (x, _)) = x
        justSnd (Just (_, y)) = y

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x   
  where justFst (Just (x, _)) = x
        justSnd (Just (_, y)) = y


data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = if isJust (f a)
             then Node (unfold f $ justFst $ f a) (justSnd $ f a) (unfold f $ justThd $ f a)
             else Leaf
  where justFst (Just (x, _, z)) = x
        justSnd (Just (_, y, _)) = y
        justThd (Just (_, _, z)) = z

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\a -> if a /= n then Just (a+1, a, a+1) else Nothing ) 0 
