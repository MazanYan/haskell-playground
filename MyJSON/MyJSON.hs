module MyJSON (
    JValue(..),
    toString,
    toInt,
    toDouble,
    toBool,
    toObject,
    toArray,
    isNull
) where

import Data.List (intercalate)

data JValue = JString String 
    | JInt Integer
    | JDouble Double 
    | JBool Bool 
    | JObject [(String, JValue)]
    | JArray [JValue]
    | JNull
    deriving (Eq, Ord)

toString :: JValue -> Maybe String 
toString (JString s) = Just s
toString _           = Nothing 

toInt :: JValue -> Maybe Integer
toInt (JInt i)    = Just i
toInt (JDouble d) = Just (floor d)
toInt _           = Nothing

toDouble :: JValue -> Maybe Double
toDouble (JDouble d) = Just d
toDouble (JInt i)    = Just (fromInteger i)
toDouble _           = Nothing

toArray :: JValue -> Maybe [JValue]
toArray (JArray vs) = Just vs
toArray _           = Nothing

toBool :: JValue -> Maybe Bool
toBool (JBool b) = Just b
toBool _         = Nothing 

toObject :: JValue -> Maybe [(String, JValue)]
toObject (JObject o) = Just o
toObject _           = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull

instance Show JValue where
    show (JDouble d) = show d
    show (JInt i) = show i
    show (JString s) = "\"" ++ s ++ "\""
    show (JBool False) = "false"
    show (JBool True) = "true"
    show JNull = "null"
    show (JArray xs) = "[" ++ intercalate ", " (map show xs) ++ "]"
    show (JObject entries) = "{ " ++ pairs entries ++ " }"
        where pair (key, val) = "\"" ++ key ++ "\": " ++ show val
              pairs xs = intercalate ", " (map pair xs)
