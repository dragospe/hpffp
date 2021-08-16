import Data.Time

data DatabaseItem =
  DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbNumber 9001
  , DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbString "Hello, world!" 
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbNumber 9006
  , DbNumber 9024
  ]

isDbDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
isDbDate item times = case item of 
  DbDate time -> times ++ [time] 
  _ -> times

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = foldr isDbDate [] items


isDbNumber item ints = case item of
  DbNumber item -> ints ++ [item]
  _ -> ints

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr isDbNumber []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb  = sum . filterDbNumber 

avgDb :: [DatabaseItem] -> Double
avgDb items = (fromIntegral $ sumDb items) / (fromIntegral $ length $ filterDbNumber items)

