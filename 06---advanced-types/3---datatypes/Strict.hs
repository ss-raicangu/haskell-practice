module Main where

import System.Environment

data SMaybe a = SNothing | SJust !a  deriving Show

main = do
  [cmd] <- getArgs
  case cmd of
    "a" -> printJust   undefined
    "b" -> printJust   Nothing
    "c" -> printJust  (Just undefined)
    "d" -> printJust  (Just ())

    "e" -> printSJust  undefined
    "f" -> printSJust  SNothing
    "g" -> printSJust (SJust undefined)
    "h" -> printSJust (SJust ())

printJust :: Maybe () -> IO ()
printJust Nothing = putStrLn "Nothing"
printJust (Just x) = do putStr "Just "; print x

printSJust :: SMaybe () -> IO ()
printSJust SNothing = putStrLn "Nothing"
printSJust (SJust x) = do putStr "Just "; print x
