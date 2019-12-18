module Main
  where

import System.IO

main = do
  hSetBuffering stdin LineBuffering
  putStrLn "What is your name?"
  name <- getLine
  if name == "Simon" || name == "John" || name == "Phil"
  then
    putStrLn "Haskell is a great programming language!"
  else
    if name == "Koen"
    then
      putStrLn "Debugging Haskell is fun!"
    else
      putStrLn "Sorry, I don't know who you are."
