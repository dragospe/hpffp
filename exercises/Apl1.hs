-- Module doesn't work; theres some weirdness going on with GHC that won't let me overwrite definitions from Control.Applicative

module Apl1 where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


newtype Zl a = Zl (ZipList)
newtype S a = S (Sum)

instance Semigroup a => Semigroup (Zl a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Zl a) where
  mempty = Zl []

instance Arbitrary a => Arbitrary (Zl a) where
  arbitrary = Zl <$> arbitrary

instance Arbitrary a => Arbitrary (S a) where
  arbitrary = S <$> arbitrary

instance Eq a => EqProp (Zl a) where
  (=-=) = eq
