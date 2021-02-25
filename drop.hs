myDrop :: Int -> [a] -> [a]
myDrop n [] = []
myDrop n xs = if n <= 0
    then xs
    else myDrop (n-1) (tail xs)

beforeTheLast :: [a] -> a
beforeTheLast []       = error "Cannot get the element before the last"
beforeTheLast [x]      = error "Cannot get the element before the last"
beforeTheLast (x : xs) = if length xs == 1
    then x
    else beforeTheLast xs
