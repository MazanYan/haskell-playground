data Tree a = Node { 
    value :: a, 
    left :: Tree a, 
    right :: Tree a 
} | Empty
    deriving (Show)

data Navigate = GoLeft | GoRight
    deriving (Eq, Show)

fromJust :: Maybe a -> a
fromJust Nothing  = error "Converting Nothing into any value"
fromJust (Just x) = x

getValue :: Tree a -> [Navigate] -> Maybe a
getValue Empty _ = Nothing
getValue tree (x:xs) = if x == GoLeft
    then getValue (left tree) xs
    else getValue (right tree) xs
getValue tree [] = Just (value tree)

height :: Tree a -> Int
height Empty = 0
height tree  = 1 + max (height (left tree)) (height (right tree))
