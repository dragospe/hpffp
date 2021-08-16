{-# LANGUAGE FlexibleInstances #-}
module Ch11 where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Size = Small | Medium | Large | Jumbo deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000) 
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Jumbo

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _  = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars [] = []
areCars x = map isCar x

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (n, s) = n > 42
  
-- with newtype
newtype Goats = Goats (Int, String) deriving Show

instance TooMany Goats where
  tooMany (Goats (n, s)) = n > 42

-- 11.3
--

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b = RecordProduct {pfirst :: a, psecond :: b} deriving (Eq, Show)

-----------
--
data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
  , lang :: ProgLang }
  deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
 [ GnuPlusLinux
 , OpenBSDPlusNevermindJustBSDStill
 , Mac
 , Windows
 ]

allLanguages :: [ProgLang]
allLanguages =
 [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]


newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show
-- FarmerType is a Sum
data FarmerType = DairyFarmer
 | WheatFarmer
 | SoybeanFarmer
 deriving Show
-- Farmer is a plain ole product of
-- Name, Acres, and FarmerType
data Farmer =
 Farmer Name Acres FarmerType
 deriving Show

data FarmerRec =
  FarmerRec { name :: Name
  , acres :: Acres
  , farmerType :: FarmerType }
  deriving Show


