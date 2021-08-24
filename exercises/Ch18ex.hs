module Ch18ex where

import Control.Monad ((>=>))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

{-
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<*>) :: Applicative (Sum c) => (Sum c) (a -> b) -> (Sum c) a -> (Sum c) b
-}

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = First a
  (Second f) <*> b = f <$> b

{-
(>>=) :: Monad f => f a -> (a -> f b) -> f b
(>>=) :: Monad (Sum c) => (Sum c) a -> (a -> (Sum c) b) -> (Sum c) b

Author's note: This was a little tricky, because I was confusing the
type for >>= with the type for <*>. Remember: in monads, the function
on the RHS of >>= is NOT in the monadic structure; it operates on raw
values and RETURNS something in the monadic structure. This is basically
the entire point!
-}
instance Monad (Sum c) where
  First c >>= _ = First c
  Second a >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (First a), return (Second b)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor (Second (3, 3, 3) :: Sum Integer (Integer, Integer, Integer))
  quickBatch $ applicative (Second (3, 3, 3) :: Sum Integer (Integer, Integer, Integer))
  quickBatch $ monad (Second (3, 3, 3) :: Sum Integer (Integer, Integer, Integer))

--- The "Bad Monad" example on pg 779

data CountMe a
  = CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

{-
I was initially very confused about the let expression below. For
future me: the thing to remember is *deconstruction*, where you can
do things like [x, y] = [1, 3] to get x = 1 and y = 3. This works
for data constructors in general, and is how we get the values of
`n'` and `b` in the let expression.
-}

instance Monad CountMe where
  return = pure
  (>>=) (CountMe n a) f =
    {- So here we're defining a function w/ 2 args,
       but we're also setting values of `n` and `a`.
     -}
    let CountMe n' b = f a {- Here, we're deconstructing the result of f a,
                              which returns a value of type CountMe b
                           -}
     in CountMe (n + n') b {- Here, we're using the value set by the
                              function parameters (n) along with the
                              values set by the `let` deconstruction (n', b)
                              and combining them -}

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main2 :: IO ()
main2 = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-----------
-- Kliesli Composition Section
-----------

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you!"

------------
-- Chapter Exercises
------------

---- Ex 1
-- Phantom type argument, so nothing happens
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

nopeTest :: IO ()
nopeTest = do
  let trigger :: Nope (Int, String, Char)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

----
-- Ex 2

data PhbtEither b a
  = L a
  | R b
  deriving (Eq, Show)

instance Functor (PhbtEither b) where
  fmap _ (R b) = R b
  fmap f (L a) = L (f a)

{-
(<*>) :: f (a -> b) -> f a -> f b
      :: (PhbtEither c) (a -> b) -> (PhbtEither c) a -> (PhbtEither c) b
-}

instance Applicative (PhbtEither c) where
  pure = L
  (R c) <*> _ = R c
  (L f) <*> a = f <$> a

{-
(>>=) :: PhbtEither c a -> (a -> PhbtEither c b) -> PhbtEither c b
-}
instance Monad (PhbtEither c) where
  (R c) >>= _ = R c
  (L a) >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    oneof [return (L b), return (R a)]

instance (Eq a, Eq b) => EqProp (PhbtEither b a) where
  (=-=) = eq

testPhbtEither :: IO ()
testPhbtEither = do
  let trigger :: PhbtEither Int (Int, Int, Int)
      trigger = undefined
  quickBatch $ functor trigger
  verboseBatch $ applicative trigger
  verboseBatch $ monad trigger

--- Ex 3

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  (Identity x) >>= f = f x

{- Was going to do:
-----------
  arbitrary = do
     a <- arbitrary
     return $ Identity a
-----
But flycheck suggested rewriting to the following.
Interesting technique that I wouldn't have thought of.
-}
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

testIdentity :: IO ()
testIdentity = do
  let trigger :: Identity (Int, Int, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

--- Ex 4

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

{-
(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: List (a -> b) -> List a -> List b
-}

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f Nil) <*> l = f <$> l
  (Cons f fs) <*> l = joinList (f <$> l) (fs <*> l)

joinList :: List a -> List a -> List a
joinList Nil x = x
joinList x Nil = x
joinList (Cons x xs) (Cons y ys) =
  Cons x (joinList xs (Cons y ys))

-- Taken from https://stackoverflow.com/questions/36055669/how-do-i-create-an-arbitrary-instance-for-a-recursive-datatype
instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [(1, nil), (4, cons)]
    where
      nil = return Nil
      cons = do
        h <- arbitrary -- This is the `Arbitrary a` instance
        Cons h <$> arbitrary {- This is clever: that second arbitrary
                                recursively invokes the Arbitrary
                                for `List a`!
                              -}

instance Eq a => EqProp (List a) where
  (=-=) = eq

{-
List a -> (a -> List b) -> List b
-}
instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = flattenList (f <$> Cons x xs)

flattenList :: List (List a) -> List a
flattenList Nil = Nil
flattenList (Cons Nil ys) = flattenList ys
flattenList (Cons (Cons x xs) ys) = Cons x (flattenList (Cons xs ys))

testList :: IO ()
testList = do
  let trigger :: List (Int, Char, Integer)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

---- "Write the function Exercises ----

-- Ex 1
-- (Just implement join)

{- What we're working with:
Monad:
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
Applicative:
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
Functor
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
-}

-- Join
j :: Monad m => m (m a) -> m a
j = (>>= id) -- Clever!

-- lifts
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = f <$> x

l2 ::
  Monad m =>
  (a -> b -> c) ->
  m a ->
  m b ->
  m c
l2 f x y = (f <$> x) <*> y

meh :: Monad m => [a] -> (a -> m b) -> m [b]
{- WRONG SOLUTION (for didactic purposes):

   meh l@(x:xs) f = return (l >>= f)

The reason its NOT this is because `f`, in this case,
would be of type `(a -> [b])`, rather than to an arbitrary
`m b`.
-}
meh [] _ = return []
meh (x : xs) f = (:) <$> f x <*> meh xs f
