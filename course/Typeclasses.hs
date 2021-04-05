module Typeclasses where

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2 

class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x 
        | doesEnrageGork x && doesEnrageMork x = stomp $ stab x
        | doesEnrageMork x = stomp x
        | doesEnrageGork x = stab x
        | otherwise = x

class (Eq a, Bounded a, Enum a) => SafeEnum a where
    ssucc :: a -> a
    ssucc x 
        | x == maxBound = minBound 
        | otherwise = succ x

    spred :: a -> a
    spred x 
        | x == minBound = maxBound  
        | otherwise = pred x
