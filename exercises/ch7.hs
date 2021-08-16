module Ch7 where

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = (\n -> n+1)

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser 
           (Username name)
           (AccountNumber acctNum)) = 
           putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive = 
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

-- Exercises: Case Practice
functionC x y = if (x > y) then x else y
functionC' x y = 
  case (x > y) of
    True -> x
    False -> y

ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2' n = 
  case even n of
    True -> n+2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Exercises: Artful Dodgy

dodgy :: (Num x) => x -> x -> x
oneIsOne :: (Num x) => x -> x
oneIsTwo :: (Num x) => x -> x

dodgy x y = x + y *  10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

add :: Int -> Int -> Int 
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO ()
main = do
  print (0 :: Int) -- 0
  print (add 1 0) -- 0
  print (addOne 0) -- 1
  print (addOnePF 0) --  1
  print ((addOne . addOne) 0) -- 2
  print (( addOnePF . addOne ) 0) -- 2
  print ((addOnePF . addOnePF) 0 ) -- 2
  print (negate (addOne 0)) -- -1
  print (( negate . addOne ) 0 ) -- -1
  print ((addOne . addOne . addOne. negate . addOne) 0) -- 2


foldBool1 :: a -> a -> Bool -> a
foldBool1 f s b = 
  case b of
    True -> f
    False -> s

foldBool2 :: a -> a -> Bool -> a
foldBool2 f s b
  | b = f
  | not b = s


roundTrip :: (Show a, Read b) => a -> b 
roundTrip a = read (show a) 

