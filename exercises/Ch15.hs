module Ch15 where

import           Data.Monoid
import           Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada         = Nada
  (<>) (Only x) Nada     = Only x
  (<>) Nada (Only y)     = Only y
  (<>) (Only x) (Only y) = Only (x <> y)


instance Monoid a => Monoid (Optional a) where
  mempty = Nada


type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj = mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

-- Quickcheck monoid properties

monoidAssoc :: (Monoid a, Eq a) => a -> a -> a -> Bool
monoidAssoc x y z = x <> (y <> z) == (x <> y) <> z

monoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
monoidLeftIdentity x = mempty <> x == x

monoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
monoidRightIdentity x = x <> mempty  == x



-- Maybe Monoid 2

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

first'Gen :: Arbitrary a => Gen (First' a)
first'Gen = do
  a <- arbitrary
  frequency [(10, return $ First' (Only a)),
             (1, return $ First' Nada)]

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = first'Gen

instance Monoid (First' a ) where
  mempty = First' Nada


instance Semigroup (First' a) where
  (<>) (First' Nada) (First' Nada)     = First' Nada
  (<>) (First' (Only x)) _             = First' (Only x)
  (<>) (First' Nada) (First' (Only y)) = First' (Only y)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
  First' String -> First' String -> First' String -> Bool
type FstId =
  First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
