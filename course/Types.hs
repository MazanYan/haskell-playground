module Types where

import Data.List (unfoldr, foldl')
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

cmp :: LogLevel -> LogLevel -> Ordering
cmp x y = compare (val x) (val y)
    where val Error = 3
          val Warning = 2
          val Info = 1

data SomeData = Value1 Int | Value2 Bool | WrongValue Char 

processData :: SomeData -> String
processData d = case doSomeWork d of
    (Success, _) -> "Success"
    (Fail, code) -> "Fail: " ++ show code

nTimes:: a -> Int -> [a]
nTimes x n = case compare n 0 of 
    GT -> x : nTimes x (n-1)
    _  -> []

data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) | a == b = True 
isSquare _                        = False

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b

data Result' = Success' | Fail' { code :: Int }

instance Show Result' where
    show Success' = "Success"
    show (Fail' code) = "Fail: " ++ show code

data Result = Fail | Success

doSomeWork :: SomeData -> (Result,Int)
doSomeWork (Value1 x) = (Success, 0)
doSomeWork (Value2 v) = (Success, 0)
doSomeWork (WrongValue _) = (Fail, 1)

doSomeWork' :: SomeData -> Result'
doSomeWork' someData = case doSomeWork someData of 
    (Success, _) -> Success'
    (Fail, code) -> Fail' code

data Bit = Zero | One deriving (Show)
data Sign = Minus | Plus deriving (Show)
data Z = Z Sign [Bit] deriving (Show)

getBits :: Integer -> [Bit]
getBits 0 = [Zero]
getBits i = map numToBit bits
    where bits = unfoldr (\a -> if a > 0 then Just (a `mod` 2, a `div` 2) else Nothing) $ abs i
          numToBit 1 = One
          numToBit 0 = Zero

zToI :: Z -> Integer
zToI (Z sign bits) = let (res, pow) = foldl' (\(res,pow) b -> (res+pow*getNum b, pow*2)) (0,1) bits
    in case sign of
        Minus -> -res
        _     -> res
    where getNum Zero = 0
          getNum One = 1

iToZ :: Integer -> Z
iToZ i 
    | i >= 0 = Z Plus $ getBits i
    | otherwise = Z Minus $ getBits i

add :: Z -> Z -> Z
add z1 z2 = iToZ . sum $ map zToI [z1, z2]

mul :: Z -> Z -> Z
mul z1 z2 = iToZ . product $ map zToI [z1, z2]

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { time :: UTCTime, level :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString entry = timeToString (time entry) ++ ": " ++ logLevelToString (level entry) ++ ": " ++ message entry

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

abbrFirstName :: Person -> Person
abbrFirstName p@Person { firstName=fn } = p { firstName = shorten fn }

shorten :: String -> String 
shorten ""         = ""
shorten str@(c:cs) = if length str >=2
    then c : "." 
    else c : ""

data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2 - x1) + abs (y2 - y1)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x
