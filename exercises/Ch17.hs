module Ch17 where

import           Data.List (elemIndex)


added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$>  y <*> z

--3

a :: Maybe Int
a = elemIndex 3 [1, 2, 3, 4, 5]

b :: Maybe Int
b = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed =  max' <$> a <*> b

-- 4

js = [1, 2, 3]
ks = [4, 5, 6]

j :: Maybe Integer
j = lookup 3 $ zip js ks

k :: Maybe Integer
k = lookup 2 $ zip js ks

summed :: Maybe Integer
summed = (sum <$>) $  (,) <$> j <*> k

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)


-- Constant; don't totally understand this one

newtype Constant a b =
  Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a


instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant (x <> y)


-- Maybe
validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = case mkName n of
  Nothing -> Nothing
  Just n' -> case mkAddress a of
    Nothing -> Nothing
    Just a' -> Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a

-- Fixer Upper

x' = const <$> Just "Hello" <*> pure "World"

y' = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
