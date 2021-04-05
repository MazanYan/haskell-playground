module FunctorsMonadsBasic where

import Data.List (foldl')

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
    fmap f (Point pt) = Point (fmap f pt)
    fmap f (LineSegment pt1 pt2) = LineSegment (fmap f pt1) (fmap f pt2)

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf v) = Leaf (fmap f v)
    fmap f (Branch l v r) = Branch (fmap f l) (fmap f v) (fmap f r)

data Entry k1 k2 v = Entry (k1, k2) v 
    deriving Show

data Map k1 k2 v = Map [Entry k1 k2 v] 
    deriving Show

instance Functor (Entry k1 k2) where
    fmap f (Entry keys v) = Entry keys (f v)

instance Functor (Map k1 k2) where
    fmap f (Map []) = Map []
    fmap f (Map entries) = Map $ fmap (fmap f) entries

-- 

data Log a = Log [String] a deriving (Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (msg1++msg2) v2
    where (Log msg1 v1) = f x
          (Log msg2 v2) = g v1

instance Functor Log where
    fmap = undefined 

instance Applicative Log where
    (<*>) = undefined 
    pure = undefined 

instance Monad Log where
    return = Log []
    (>>=) = bindLog

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log mgs1 v1) f = Log (mgs1++mgs2) v2 
    where (Log mgs2 v2) = f v1

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList v (lFun1:lFuns) = helper log1 lFuns 
    where log1 = lFun1 v
          helper l logFuns'' = foldl' (>>=) l logFuns''

appl = (+) <$> Just 3
extracted = let (Just fun) = appl
    in fun

-- Write Functor instance of an unknown type knowing it's an instance of Monad
data SomeType a = SomeType a 

instance Functor SomeType where
    fmap f v = v >>= f'
        where f' = \x -> return $ f x 

instance Applicative SomeType where
    pure = undefined 
    (<*>) = undefined 

instance Monad SomeType where
    return = undefined 
    (>>=) = undefined 
