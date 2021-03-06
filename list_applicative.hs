module ListApplicative where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



---- List applicative exercises
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)


gen4List :: (Arbitrary a)
         => Gen (List a)
gen4List = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Cons a (Cons b (Cons c (Cons d Nil))))
  
instance Arbitrary (List a) where
  arbitrary = (gen4List :: Arbitrary (List Int))

instance Semigroup (List a) where
  Nil <> y = y
  y <> Nil = y
  Cons x x' <> y = Cons x (x' <> y)

{-
instance Monoid (List a) where
  mempty = Nil
  
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Applicative List where
  pure x = Cons x Nil
  f <*> Nil = Nil
  f <*> (Cons x y) = Cons (f x) (f <*> y)
-}





-- main :: IO ()
-- main = quickBatch (semigroup (Cons 2 Nil))
