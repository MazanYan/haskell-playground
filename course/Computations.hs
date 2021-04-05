module Computations where

import Data.Char (isDigit, digitToInt)

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if all isDigit [x, y]
    then read [x, y]
    else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt $ (x2-x1)**2 + (y2-y1)**2

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

-- bad solution both for positive and negative numbers
fibonacci :: Integer -> Integer 
fibonacci n
    | n > 0 = fibonacci' 1 0 1
    | n == 0 = 0
    | n < 0 = fibonacci'' (-1) 0 1
        where fibonacci' i x2 x1 = if i == n 
                then x1
                else fibonacci' (i+1) x1 (x1+x2)
              
              fibonacci'' i x2 x1 = if i == n
                then x1
                else fibonacci'' (i-1) x1 (x2-x1)

-- Taylor sequence coefficients
-- of function f(x) = (1 + x)/(1 - x - x^2 + 2x^3)
seqA :: Integer -> Integer 
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = helper (seqA 0, seqA 1, seqA 2) 2
    where helper (r3, r2, r1) i
            | i == n = r1
            | otherwise = helper (r2, r1, r1 + r2 - 2*r3) (i+1)

-- find sum and count of number's digits and
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n = let xs = show n
                in helper (0, 0) xs
                where 
                    helper (s, c) []     = (s, c)
                    helper (s, c) (y:ys) 
                        | isDigit y = helper (s + toInteger (digitToInt y), c+1) ys 
                        | otherwise = helper (s, c) ys

range :: (Enum a, Fractional a) => a -> a -> a -> ([a], a)
range a1 a2 n = let step = (a2 - a1) / n
                in ([a1+step, a1+2*step.. a2-step], step)

-- Integration using the Trapezoidal rule
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b 
    | a /= b = h  * ((fx0 + fxn) / 2 + sum fxi)
    | otherwise = 0
    where (xi, h) = range a b 1000
          fxi = map f xi
          fx0 = f a
          fxn = f b
