{-# LANGUAGE MultiParamTypeClasses #-}

module Ch16 where

import           Data.Char
import           Test.QuickCheck
import           Test.QuickCheck.Function

class Else where
  e :: b -> f (g a b c)


replaceWithP :: b -> Char
replaceWithP = const 'p'

-- lms ~ list of Maybe Strings
lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a ) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main = do
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms) -- 'p'

  putStr "liftedReplace lms: "
  print (liftedReplace lms) -- ['p', 'p','p']

  putStr "liftedReplace' lms: "
  print (liftedReplace' lms) --

  putStr "twiceLifted lms: "
  print (twiceLifted lms) -- [Just 'p', Nothing, Just 'p']

  putStr "twiceLifted' lms: "
  print (twiceLifted' lms)

  putStr "thriceLifted lms: "
  print (thriceLifted lms) -- [Maybe 'ppp', Nothing, Maybe 'pppppp']

  putStr "thriceLifted' lms: "
  print (thriceLifted' lms)



f :: IO Integer
f = let ioi = readIO "1" :: IO Integer
        changed =  (fmap read  (fmap ("123" ++)  (fmap show ioi)))
    in fmap (*3) changed


g = let ioi = readIO "1" :: IO Integer
    in  fmap ("123" ++) (fmap show ioi)

-- Functor QuickCheck Properties

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

{- I've seen the statement "a functor is a container" before in terms of haskell, but this seems
   maybe inaccurate? Like... I feel like "List" isn't a functor; "List" just has a functor-instance
   defined on it. Functors are functions that preserve structure.


   When we do something like

   `fmap f [1,2,3]`, is `f` the functor, or is `[]`, or neither?

   I feel like `f` is just a function, `[]` is the "structure", and the "technique" of applying `f` to each element of `[]`, combined
   with `f` itself is "the functor".

  "A functor is a higher-kinded type equipped with a higher-order lifting function."

  So "Maybe" *is* actually a functor, because it maps types to types and is equipped with a way to map morphisms in the domain type to the codomain type.

-}

type I2I = Fun Int Int

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = fmap (g . f) x == (fmap g . fmap f $ x)

---------
-- 16.10: Exercises implementing Functors
---------


-- 16.10.1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- 16.10.2

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y


-- 16.10.3

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

-- 16.10.4

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- 16.10.5

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z


-- 16.10.6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

-- 16.10.7

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z



check :: IO ()
check = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorCompose' :: Identity Int -> I2I -> I2I-> Bool)

  quickCheck (functorIdentity :: Pair Int  -> Bool)
  quickCheck (functorCompose' :: Pair Int -> I2I -> I2I -> Bool)

  quickCheck (functorIdentity :: Two Int Char  -> Bool)
  quickCheck (functorCompose'  :: Two Int Int -> I2I -> I2I  -> Bool)

  quickCheck (functorIdentity :: Three Int Char String  -> Bool)
  quickCheck (functorCompose' :: Three Int Char Int -> I2I -> I2I -> Bool)

  quickCheck (functorIdentity :: Three' Int String -> Bool)
  quickCheck (functorCompose' :: Three' Char Int -> I2I -> I2I -> Bool)

  quickCheck (functorIdentity :: Four Int String Char Bool-> Bool)
  quickCheck (functorCompose' :: Four Char Int String Int-> I2I -> I2I -> Bool)


  quickCheck (functorIdentity :: Four' Int String -> Bool)
  quickCheck (functorCompose' :: Four' Char Int -> I2I -> I2I -> Bool)

  quickCheck (functorIdentity :: Possibly Int -> Bool)
  quickCheck (functorCompose' :: Possibly Int -> I2I -> I2I -> Bool)

  quickCheck (functorIdentity :: Sum String Int -> Bool)
  quickCheck (functorCompose' :: Sum String Int -> I2I -> I2I -> Bool)

-- Exercise: Possibly

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return LolNope), (10, return (Yeppers x))]

-- Exercise: Sum a b

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a)  = First a
  fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof [return $ First x, return $ Second y]


