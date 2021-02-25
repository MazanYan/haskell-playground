data Tree a = Node { 
    value :: a, 
    left :: Tree a, 
    right :: Tree a 
} | Empty
    deriving (Show)

data Navigate = GoLeft | GoRight
    deriving (Eq, Show)

getValue :: Tree a -> [Navigate] -> a
getValue Empty _ = error "Too many directions"
getValue tree (x:xs) = if x == GoLeft
    then getValue (left tree) xs
    else getValue (right tree) xs
getValue tree [] = value tree
