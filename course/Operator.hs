module Operator where

infixl 7 |-|
(|-|) :: Num a => a -> a -> a
(|-|) a b = abs $ a - b
