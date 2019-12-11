module Main
    where

import System.IO

main = do
    hSetBuffering stdin LineBuffering                    -- Only necessary for GHC, because it reads in large blocks. We want only a single line.
    putStrLn "Please enter your name: "
    name <- getLine                                      -- Using the arrow shows that getLine is not a pure function and can return different values.
    putStrLn ("Hello, " ++ name ++ ", how are you?")
