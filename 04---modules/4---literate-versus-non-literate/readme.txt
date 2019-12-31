=================================================================================================
 https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Modules#Literate_Versus_Non-Literate
-------------------------------------------------------------------------------------------------
 Accessed at 2019-12-31T18:26:41+1300
=================================================================================================



The literate programming style is to simply reverse the way comments and code is written, i.e., code is annotated with a special character while comments are written without any annotation. There are two types: Bird-scripts and LaTeX-scripts.
Literate scripts have the     .lhs     extension in Haskell.




__________________________________________________________________________________
                                   Bird-scripts
``````````````````````````````````````````````````````````````````````````````````
Comments are default. Code is introduced with a leading greater-than sign     ">".

======================================================
 Example of the     "Hello World"     program
------------------------------------------------------
 This is a simple (literate!) Hello World program.

 > module Main
 >     where

 All our main function does is print a string:

 > main = putStrLn "Hello World"
------------------------------------------------------
 Extra lines between comments and code is compulsory.
======================================================




_______________________________________________________________
                         LaTeX-scripts
```````````````````````````````````````````````````````````````
Basically like LaTeX with seemingly one environment     {code}.

==========================================================
 Example of the     "Hello World"     program
----------------------------------------------------------
 This is another simple (literate!) Hello World program.

 \begin{code}
 module Main
     where
 \end{code}

 All our main function does is print a string:

 \begin{code}
 main = putStrLn "Hello World"
 \end{code}
----------------------------------------------------------
 Extra lines between comments and code is not compulsory.
==========================================================
