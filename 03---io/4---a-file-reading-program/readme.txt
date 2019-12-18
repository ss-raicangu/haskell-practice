======================================================================================
 https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Io#A_File_Reading_Program
--------------------------------------------------------------------------------------
 Accessed at 2019-12-17T08:58:27
======================================================================================



brackets     function doesn't catch exceptions in the first two arguments, which is the code run at the start and end of execution.




_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
Write a program that first asks whether the user wants to read from a file, write to a file or quit. If the user responds quit, the program should exit. If he responds read, the program should ask him for a file name and print that file to the screen (if the file doesn't exist, the program may crash). If he responds write, it should ask him for a file name and then ask him for text to write to the file, with "." signaling completion. All but the "." should be written to the file. For example, running this program might produce:

=========================================================
 Example:
---------------------------------------------------------
 Do you want to [read] a file, [write] a file or [quit]?
 read
 Enter a file name to read:
 foo
 ...contents of foo...
 Do you want to [read] a file, [write] a file or [quit]?
 write
 Enter a file name to write:
 foo
 Enter text (dot on a line by itself to end):
 this is some
 text for
 foo
 .
 Do you want to [read] a file, [write] a file or [quit]?
 read
 Enter a file name to read:
 foo
 this is some
 text for
 foo
 Do you want to [read] a file, [write] a file or [quit]?
 read
 Enter a file name to read:
 foof
 Sorry, that file does not exist.
 Do you want to [read] a file, [write] a file or [quit]?
 blech
 I don't understand the command blech.
 Do you want to [read] a file, [write] a file or [quit]?
 quit
 Goodbye!
=========================================================

`````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
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



ACTUAL SOLUTION
~~~~~~~~~~~~~~~

============================================================================================================================
 module DoFile where

 import IO

 main = do
   hSetBuffering stdin LineBuffering
   putStrLn "Do you want to [read] a file, ...?"
   cmd <- getLine
   case cmd of
     "quit"  -> do putStrLn ("Goodbye!")
                   return ()
     "read"  -> do doRead; main
     "write" -> do doWrite; main
     _       -> do putStrLn
                     ("I don't understand the command "
                      ++ cmd ++ ".")
                   main

 doRead = do
   putStrLn "Enter a file name to read:"
   fn <- getLine
   bracket (openFile fn ReadMode) hClose
           (\h -> do txt <- hGetContents h
                     putStrLn txt)

 doWrite = do
   putStrLn "Enter a file name to write:"
   fn <- getLine
   bracket (openFile fn WriteMode) hClose
           (\h -> do putStrLn
                       "Enter text (dot on a line by itself to end):"
                     writeLoop h)

 writeLoop h = do
   l <- getLine
   if l == "."
     then return ()
     else do hPutStrLn h l
             writeLoop h
----------------------------------------------------------------------------------------------------------------------------
 Alternate ways to write     doRead,     doWrite,     writeLoop/getWriteLines     using     readFile     and     writeFile.

 doRead = do
   putStrLn "Enter a file name to read:"
   fn <- getLine
   txt <- readFile fn
   putStr txt

 doWrite = do
   putStrLn "Enter a file name to write:"
   fn <- getLine
   txt <- getWriteLines
   writeFile fn txt

 getWriteLines = do
   l <- getLine
   if l == "."
     then return ""
     else do lines <- getWriteLines
             return (line++"\n"++lines)
============================================================================================================================
