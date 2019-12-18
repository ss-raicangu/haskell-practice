=======================================================================
 https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Io#Actions
-----------------------------------------------------------------------
 Accessed at 2019-12-16T12:08:58
=======================================================================



========================================================================================
 Two common IO actions are:
----------------------------------------------------------------------------------------
 putStrLn :: String -> IO ()
 getLine :: IO String
----------------------------------------------------------------------------------------
 "IO ()"     means that an IO action is run, the result of which is the unit type "()".
 "IO String"     means the IO action returns a String.
========================================================================================



<-     is used to get a value from an action.
The     if/then/else     construction needs to have the same type for the     then     branch and the     else     branch. It has to have the     Bool     type for the condition in the     if     branch. The type of the entire construction is therefore the type of the two    then/else     branches.
The     do     notation is used to sequence actions. Superfluous if there is only one action to take.



========================================================================================================================================================================================================================================
 do if (read guess) < num
          then putStrLn "Too low!"
               doGuessing num
          else ...
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 In the above sequence, not including a     do     in the     then     branch will make the compiler think that     putStrLn     is taking 3 arguments: the String "Too low!", the function     doGuessing,     and an integer     num.
========================================================================================================================================================================================================================================



case     is the     switch     of the Haskell language.
return     doesn't work as expected in Haskell. Instead of returning a value and exiting the function, it takes a normal value (e.g. Int) and makes it into an action returning the same value (e.g. IO Int).




___________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
Write a program that asks the user for his or her name. If the name is one of Simon, John or Phil, tell the user that you think Haskell is a great programming language. If the name is Koen, tell them that you think debugging Haskell is fun (Koen Classen is one of the people who works on Haskell debugging); otherwise, tell the user that you don't know who he or she is. Write two different versions of this program, one using if statements, the other using a case statement.
```````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
==========================================================
 Program using the     if/then/else     construct
----------------------------------------------------------
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
==========================================================



====================================================================
 Program using the     case     construct
--------------------------------------------------------------------
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
====================================================================



RELATED NOTES
~~~~~~~~~~~~~

==============================================================
 Alternative ways to write the     if/then/else     construct
--------------------------------------------------------------
 main = do
   hSetBuffering stdin LineBuffering
   putStrLn "Please enter your name:"
   name <- getLine
   if name `elem` ["Simon", "John", "Phil"]
     then putStrLn "Haskell is great!"
     else if name == "Koen"
            then putStrLn "Debugging Haskell is fun!"
            else putStrLn "I don't know who you are."
--------------------------------------------------------------
 main = do
   hSetBuffering stdin LineBuffering
   putStrLn "Please enter your name:"
   name <- getLine
   putStrLn
     (if name `elem` ["Simon", "John", "Phil"]
        then "Haskell is great!"
        else if name == "Koen"
               then "Debugging Haskell is fun!"
               else "I don't know who you are.")
==============================================================
