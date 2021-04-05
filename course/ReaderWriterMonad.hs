module ReaderWriterMonad where

import Data.Monoid (Sum (Sum), getSum)

-- w - log type; a - computation result 
newtype Writer w a = Writer { runWriter :: (a, w) }

writer :: (a, w) -> Writer w a
writer = Writer


-- returns log of writer after computation
execWriter :: Writer w a -> w
execWriter = snd . runWriter

-- returns computation value only
evalWriter :: Writer w a -> a
evalWriter = fst . runWriter

instance (Monoid m) => Functor (Writer m) where 
    fmap f w = let (x, m1) = runWriter w
        in writer (f x, m1)

instance (Monoid m) => Applicative (Writer m) where
    pure = return 
    (<*>) w1 w2 = let (f, m1) = runWriter w1
                      (x, m2) = runWriter w2
        in writer (f x, m1 `mappend` m2)

instance (Monoid m) => Monad (Writer m) where
    return v = writer (v, mempty)
    (>>=) m k = let
            (x, m1) = runWriter m
            (y, m2) = runWriter $ k x
        in writer (y, m1 `mappend` m2)

type Shopping = Writer (Sum Integer, [String]) ()

purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), (Sum cost, [item]))

total :: Shopping -> Integer
total = getSum . fst . execWriter

items :: Shopping -> [String]
items = snd . execWriter 

shopping :: Shopping
shopping = do
    purchase "Jeans"   19200
    purchase "Water"     180
    purchase "Lettuce"   328

data Reader r a = Reader { runReader :: (r -> a) }

instance Functor (Reader r) where
    fmap = undefined 

instance Applicative (Reader r) where
    pure = undefined 
    (<*>) = undefined 

instance Monad (Reader r) where
    return x = Reader $ \_ -> x
    m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r
