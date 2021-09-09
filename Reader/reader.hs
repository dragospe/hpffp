{-# LANGUAGE InstanceSigs #-}

module HpffpReader where

import Control.Applicative
import Data.Char

-- First section code; Demonstrating functor, applicative, and monad for functions.

boop :: Num a => a -> a
boop = (* 2)

doop :: Num a => a -> a
doop = (+ 10)

bip :: Num a => a -> a
bip = boop . doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

-- "Short Exercise: Warming Up"

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- cap
  b <- rev
  return (a, b)

-- "Exercise: Ask"
-- Do note: the Reader data constructor doesn't appear to exist anymore.

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \r -> f (ra r)

ask :: Reader a a
ask = Reader id

-- Exercise: Reading Comprehension

{- Recall:
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
-}
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g fa fb = g <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r
