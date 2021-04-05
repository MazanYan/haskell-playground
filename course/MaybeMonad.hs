module MaybeMonad where

import Data.Char (isDigit)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken s 
    | s == "+" = Just Plus
    | s == "-" = Just Minus
    | s == "(" = Just LeftBrace
    | s == ")" = Just RightBrace
    | all isDigit s = Just (Number (read s::Int))
    | otherwise = Nothing 

-- returns Just [Token] if string is tokenized correctly
-- Nothing if not all words are tokens
tokenize :: String -> Maybe [Token]
tokenize s = let ws = words s
    in sequence $ map asToken ws

-- Does the same as tokenize using 'do' notation
tokenize' :: String -> Maybe [Token]
tokenize' s = foldr f (return []) (words s)
    where f word list = do
            token <- asToken word 
            tokens <- list
            return (token:tokens)

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x 
    | x <= 0 = []
    | otherwise = [(a, b, c) | a <- [1..x], b <- [1..x], c <- [1..x], a^2+b^2==c^2, a < b] 

-- The same using 'do' notation
pythagoreanTriple' :: Int -> [(Int, Int, Int)]
pythagoreanTriple' x = do
    True <- [x > 0]
    a <- [1..x]
    b <- [a..x]
    c <- [b..x]
    True <- [a^2+b^2==c^2]
    True <- [a < b]
    return (a,b,c)
