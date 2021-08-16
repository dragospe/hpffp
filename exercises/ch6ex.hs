module Ch6ex where

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
                  then Blah
                  else x

type Subject = String
type Verb = String
type Object = String

data Sentence = 
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


data Rocks = 
  Rocks String deriving (Eq, Show)

data Yeah = 
  Yeah Bool deriving (Eq, Show)

data Papu = 
  Papu Rocks Yeah
  deriving (Eq, Show)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

i :: Num a => a
i = 1

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk c a b = c a == b

arith :: Num b
  => (a -> b) -> Integer -> a -> b
arith f i a = (
