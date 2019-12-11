module Main
    where

import System.IO
import Data.Foldable

askForNum = do
    putStrLn "Give me a number (or 0 to stop):"
    numStr <- getLine
    let num = read numStr
    if num == 0
      then return []
      else do
        rest <- askForNum
        return (num : rest)

factorial 1 = 1
factorial n = n * factorial (n-1)

printFactorial x = putStrLn(show(x) ++ " factorial is " ++ show(factorial x))

main = do
    numbers <- askForNum
    putStrLn("The sum is " ++ show(foldl (+) 0 numbers))
    putStrLn("The product is " ++ show(foldl (*) 1 numbers))
    forM_ numbers printFactorial
