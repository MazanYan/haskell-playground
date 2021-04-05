module RecursiveTypes where

import Data.Function (on)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

p :: Person
p = Person "A" "B" 1

parsePerson :: String -> Either Error Person
parsePerson s = 
    let records = lines s

    in if length records < 3
        then Left ParsingError
        else Right p

eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons v vs) = v : fromList vs

toList :: [a] -> List a
toList = foldr Cons Nil

data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat n | n < 0 = error "Negative values are not accepted"
        | n == 0 = Zero
        | otherwise = Suc . toNat $ n-1

add :: Nat -> Nat -> Nat
add n1 Zero = n1
add Zero n2 = n2
add n1 (Suc n) = add (Suc n1) n

mul :: Nat -> Nat -> Nat
mul n1 Zero = Zero
mul Zero n2 = Zero
mul n1 n2 = let lst = replicate (fromInteger $ fromNat n1) n2
      in foldr1 add lst

fac :: Nat -> Nat
fac Zero = Suc Zero
fac n1@(Suc nm1) = mul n1 $ fac nm1

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

height :: Tree a -> Int
height (Leaf _) = 0
height (Node t1 t2) = 1 + (max `on` height) t1 t2

size :: Tree a -> Int
size (Leaf _) = 1
size (Node t1 t2) = 1 + size t1 + size t2

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf val) = (1,val)
    go (Node t1 t2) = 
            let (n1,val1) = go t1
                (n2,val2) = go t2
            in (n1+n2,val1+val2)
