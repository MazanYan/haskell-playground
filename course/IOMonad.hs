module IOMonad where

import System.Directory (getDirectoryContents, removeFile)
import Data.List (isInfixOf, foldl')
import System.IO (hFlush, stdout)

-- remove contents of directory by file name substring
main :: IO ()
main = do
    putStr "Substring: "
    hFlush stdout
    filename <- getLine
    contents <- getDirectoryContents "."
    let matchingStrings = if filename /= "" 
        then filter (isInfixOf filename) contents
        else []
    removeFilesByName matchingStrings

removeFilesByName :: [FilePath] -> IO ()
removeFilesByName [] = putStrLn "Cancelled" 
removeFilesByName fs = mapM_ f fs
    where
        f file = do
            removeFile file
            putStrLn $ "Removing file: " ++ file

-- 
readName :: IO String
readName = do
    putStrLn "What is your name?"
    putStrLn "Name: "
    name <- getLine
    if name == ""
        then do 
            readName
        else return name

main' :: IO ()
main' = do
    name <- readName 
    putStrLn $ "Hi, " ++ name ++ "!"
