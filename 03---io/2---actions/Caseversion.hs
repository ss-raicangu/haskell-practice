module Main
  where

import System.IO

main = do
  hSetBuffering stdin LineBuffering
  putStrLn "What is your name?"
  name <- getLine
  case name of
    "Simon" -> putStrLn "Haskell is a great programming language!"
    "John"  -> putStrLn "Haskell is a great programming language!"
    "Phil"  -> putStrLn "Haskell is a great programming language!"
    "Koen"  -> putStrLn "Debugging Haskell is fun!"
    _       -> putStrLn "Sorry, I don't know who you are."
