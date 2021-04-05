on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

swap' :: (a, b) -> (b, a)
swap' (a, b) = uncurry (flip (,)) (a, b)

sndHead :: [(a, c)] -> c
sndHead = snd . head

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) 
    | odd x = x : oddsOnly xs
    | otherwise = oddsOnly xs

sum2 :: Num a => [a] -> [a] -> [a]
sum2 xs [] = xs
sum2 [] ys = ys
sum2 (x:xs) (y:ys) = (x+y) : sum2 xs ys 

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] ys zs = sum2 ys zs
sum3 xs [] zs = sum2 xs zs
sum3 xs ys [] = sum2 xs ys
sum3 (x:xs) (y:ys) (z:zs) = (x+y+z) : sum3 xs ys zs

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs = let (good, other) = span (== head xs) xs
    in good : groupElems other

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f g = filter (\x -> f x || g x) 

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = let le = filter (<=x) xs
                   gt = filter (>x)  xs
               in qsort le ++ [x] ++ qsort gt

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

insertInAllPositions :: a -> [a] -> [[a]]
insertInAllPositions x [] = [[x]]
insertInAllPositions x arr@(y:ys) = (x : arr) : map (y:) (insertInAllPositions x ys)

perms :: [a] -> [[a]]
perms [] = []
perms [x] = [[x]]
perms (x:xs) = concatMap (insertInAllPositions x) (perms xs)
