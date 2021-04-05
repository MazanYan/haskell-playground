{-# LANGUAGE InstanceSigs #-}
module MapLike where

import Prelude hiding (lookup)
import Data.Maybe (isJust)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []

    lookup :: Ord k => k -> ListMap k v -> Maybe v
    lookup key (ListMap []) = Nothing 
    lookup key (ListMap ((k,v):tail))
        | k == key = Just v
        | otherwise = lookup key (ListMap tail)

    insert :: Ord k => k -> v -> ListMap k v -> ListMap k v
    insert key valNew (ListMap lst) = 
        let 
            exists = isJust $ lookup key (ListMap lst)
        in if exists 
            then ListMap (foldr reducer [] lst) 
            else ListMap ((key,valNew):lst) 
        where reducer (key',valOld) lst' = if key==key' then (key',valNew):lst' else (key',valOld):lst'

    delete :: Ord k => k -> ListMap k v -> ListMap k v
    delete key (ListMap lst) = ListMap $ foldr reducer [] lst
        where reducer (key',val') lst' = if key==key' then lst' else (key',val'):lst' 

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap (const Nothing)

    lookup key (ArrowMap f) = f key 

    insert key value (ArrowMap f) = ArrowMap newFun 
        where newFun key' = if key'==key 
                then Just value 
                else f key'

    delete key (ArrowMap f) = ArrowMap newFun 
        where newFun key' = if key'==key 
                then Nothing 
                else f key'

    fromList lst = ArrowMap f
        where 
            f key = foldr (\(k,v) val -> if key==k then Just v else val) Nothing lst

testFun :: Int -> Maybe String
testFun k = case k of
    1 -> Just "Jaydor"
    2 -> Just "Elinevon"
    140 -> Just "Grascler"
    _ -> Nothing

insert' :: Eq k => k -> v -> (k -> v) -> (k -> v)
insert' key value f = newFun 
        where newFun key' = if key'==key 
                then value 
                else f key'
