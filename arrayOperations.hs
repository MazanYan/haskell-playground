import Data.List (sortBy)

myLength :: [a] -> Int
myLength (x:xs) = myLength xs + 1
myLength []     = 0

mySum :: Num a => [a] -> a
mySum (x:xs) = mySum xs + x
mySum []     = 0

myMean :: Fractional a => [a] -> a
myMean arr = mySum arr / fromIntegral len
    where len = myLength arr

myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse []     = []

palindrome :: [a] -> [a]
palindrome arr = arr ++ myReverse arr

myLast :: [a] -> a
myLast = head . myReverse

isPalindrome :: Eq a => [a] -> Bool 
isPalindrome (x:xs)
    | null xs   = True
    | x == last = isPalindrome rest
    | x /= last = False
    where (last:rest) = myReverse xs
isPalindrome [] = True

sortBySubArrayLength :: [[a]] -> [[a]]
sortBySubArrayLength = sortBy (\ a b -> compare (length a) (length b))

join :: [[a]] -> [a] -> [a]
join (x:xs) sep
    | null xs   = x
    | otherwise = x ++ sep ++ join xs sep
join [] _       = []
