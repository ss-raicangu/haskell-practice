==========================================================================================
 https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Language_basics#Interactivity
------------------------------------------------------------------------------------------
 Accessed at 2019-11-28T07:41
==========================================================================================

====================================
 Compiled the Name program by using
------------------------------------
 ghc --make Name.hs -o name.exe
====================================

Could have also run the program interactively by running main in ghci after loading the module.





=======================================
 Tested the Guess program by using
---------------------------------------
 stack runghc -- Guess.hs -o guess.exe
=======================================




____________________________________________________________________________________________________________
                                               MISCELLANEOUS
````````````````````````````````````````````````````````````````````````````````````````````````````````````
When trying to run/compile the Guess program, an error stating "no module named System.Random" is occurring.

Ran---stack install random---to fix the unknown module System.Random error.
It locally initialised a GHC environment with MSYS2 and other stuff at---C:\Users\srisa\AppData\Local




________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
Write a program that will repeatedly ask the user for numbers until she types in zero, at which point it will tell her the sum of all the numbers, the product of all the numbers, and, for each number, its factorial. For instance, a session might look like:

  Example:

    Give me a number (or 0 to stop):
    5
    Give me a number (or 0 to stop):
    8
    Give me a number (or 0 to stop):
    2
    Give me a number (or 0 to stop):
    0
    The sum is 15
    The product is 80
    5 factorial is 120
    8 factorial is 40320
    2 factorial is 2

Hint: Write an IO action that reads a number and, if it's zero, returns the empty list. If it's not zero, it recurses itself and then makes a list out of the number it just read and the result of the recursive call.
Hint: You will need to make use of "show" to print a number. putStrLn("Number " ++ show(n))
````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
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



PROPER SOLUTION | Accessed at 2019-11-30T02:36 from https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Language_basics/Solutions#Interactivity
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

module Main
    where

import System.IO

main = do
  nums <- getNums
  putStrLn ("The sum is " ++ show (sum nums))
  putStrLn ("The product is " ++ show (product nums))
  showFactorials nums

getNums = do
  putStrLn "Give me a number (or 0 to stop):"
  num <- getLine
  if read num == 0
    then return []
    else do rest <- getNums
            return ((read num :: Int):rest)

showFactorials []     = return ()
showFactorials (x:xs) = do
  putStrLn (show x ++ " factorial is " ++
            show (factorial x))
  showFactorials xs

factorial 1 = 1
factorial n = n * factorial (n-1)
