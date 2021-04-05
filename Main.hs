module Main where

main :: IO ()
main = undefined 

-- fmap succ "abc"                  (Char -> Char) -> [Char] -> [Char]

--                                  f a: (Char -> Char) -> [Char] -> [Char]
-- fmap succ (fmap succ "abc")      Functor f => (a -> b) -> f a -> f b

-- fmap succ succ                   Enum b => b -> b
-- fmap                             Functor f => (a -> b) -> f a -> f b
