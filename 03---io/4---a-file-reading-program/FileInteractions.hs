module Main
  where

import System.IO

main = do
  hSetBuffering stdin LineBuffering
  putStrLn "Do you want to [read] a file, [write] a file or [quit]?"
  action <- getLine
  case action of
    "read"  -> do putStrLn "Enter a file name to read:"
                  filename <- getLine
                  readFrom filename
                  main
    "write" -> do putStrLn "Enter a file name to write:"
                  filename <- getLine
                  writeTo filename
                  main
    "quit"  -> return ()
    _       -> do putStrLn ("I don't understand the command " ++ action)
                  main

readFrom filename = do
  contents <- readFile filename
  putStrLn contents

writeTo filename = do
  putStrLn "Enter text (dot on a line by itself to end):"
  contents <- readStdin
  writeFile filename contents

readStdin = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- readStdin
      return (line ++ ('\n' : rest))
