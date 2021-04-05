module ListOperations where

import Data.Char (isUpper, chr, ord)
import Data.List (unfoldr)

delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> x `max` y `max` z)

fibStream :: [Integer]
fibStream = fibonacci 0 1
    where fibonacci r1 r = r1 : fibonacci r (r+r1)

coins :: [Integer]
coins = [1, 2, 5]

change :: (Ord a, Num a) => a -> [a] -> [[a]]
change v coins | v < 0 = []
               | v == 0 = [[]]
               | otherwise = [x : y | x <- coins, y <- change (v-x) coins]

lengthList :: Integral b => [a] -> b
lengthList = foldr (\_ s -> s+1) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x+s else s) 0

meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (s,l) -> (s+x,l+1)) (0,0)

evenOnly :: [a] -> [a]
evenOnly = foldr (\(i,x) l -> if even i then x:l else l) [] . zip [1..]

lastElem :: [a] -> a
lastElem = foldl1 (\_ x -> x)

revRange :: (Char,Char) -> [Char]
revRange (start,end) = unfoldr g end
  where g c
            | c >= start = Just (c, chr $ ord c - 1)
            | otherwise  = Nothing
