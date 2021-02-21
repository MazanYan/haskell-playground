myDrop :: Int -> [a] -> [a]
myDrop n [] = []
myDrop n xs = if n <= 0
    then xs
    else myDrop (n-1) (tail xs)
