import Data.List (sortBy, foldl')
import Data.Maybe (fromJust, isNothing)

myLength :: [a] -> Int
myLength (x:xs) = myLength xs + 1
myLength []     = 0

mySum :: Num a => [a] -> a
mySum (x:xs) = mySum xs + x
mySum []     = 0

myMean :: Fractional a => [a] -> a
myMean arr = mySum arr / fromIntegral (myLength arr)

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

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs)
    | not (null xs) = Just xs
    | otherwise     = Nothing 
safeTail []         = Nothing 

safeLast :: [a] -> Maybe a
safeLast arr = safeHead (myReverse arr) 

safeInit :: [a] -> Maybe [a]
safeInit []             = Nothing
safeInit arr
    | length arr == 1   = Nothing
    | otherwise         = Just (myReverse xs)
    where (x:xs) = myReverse arr

-- acts similar to 'words' but separates
-- its input list on every element for which
-- the predicate returns False
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith pred arr
    | null rest = [word]
    | otherwise = (word ++ [head rest]) : splitWith pred (tail rest)
    where (word, rest) = span pred arr

myConcat :: [[a]] -> [a]
myConcat = foldr concat' []
    where concat' [] ys = ys
          concat' x  ys = x ++ ys

takeWhileRecursion :: (a -> Bool) -> [a] -> [a]
takeWhileRecursion f []     = []
takeWhileRecursion f (x:xs) = if f x
    then x : takeWhileRecursion f xs
    else []

takeWhileFoldR :: (a -> Bool) -> [a] -> [a]
takeWhileFoldR _ [] = []
takeWhileFoldR f xs = let (zs, shouldTerminate) = foldr take ([], False) xs
                      in zs
    where take x (ys, False) = if f x
            then (x : ys, False)
            else (ys, True)
          take _ (ys, True)  = (ys, True)

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f xs = let (zs, old) = foldl' group ([], Nothing) xs
                 in if isNothing old
                    then zs
                    else zs ++ [[fromJust old]]
    where group (ys, Just old) x = if f old x
            then (ys ++ [old : [x]], Nothing)
            else (ys ++ [[old]], Just x)
          group (ys, Nothing) x = (ys, Just x)

myCycle :: [a] -> [a]
myCycle (x1:x2:xs) = x1 : myCycle (x2 : xs ++ [x1])

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr any' False
    where any' x False = f x
          any' _ True = True

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr all' True
    where all' x True = f x
          all' _ False = False

mapEither :: (a -> Either b c) -> [a] -> Either b [c]
mapEither f (x:xs) = case mapEither f xs of 
    Left error -> Left error
    Right ys   -> case f x of
                        Left error -> Left error
                        Right y    -> Right (y:ys)
mapEither _ [] = Right []
