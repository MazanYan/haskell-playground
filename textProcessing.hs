import Data.Char (digitToInt)
import GHC.TypeLits (ErrorMessage)

splitLines :: String -> [String]
splitLines [] = []
splitLines cs = let (prefix, suffix) = break isLineTerminator cs
    in prefix : case suffix of
        ('\r':'\n':rest) -> splitLines rest
        ('\r':rest) -> splitLines rest
        ('\n':rest) -> splitLines rest
        _           -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'

fixLines :: String -> String 
fixLines input = unlines (splitLines input)

isDigit :: Char -> Bool 
isDigit c = c `elem` "123456789"

asInt :: String -> Int
asInt str = let (pow, num) = foldr stepToDigit (0, 0) str
            in num
    where stepToDigit '-' (pow, num) = (pow, negate num)
          stepToDigit c (pow, num) = 
              if isDigit c
              then (pow+1, (10^pow) * digitToInt c + num)
              else error ('\'' : c : "' is not a digit")
