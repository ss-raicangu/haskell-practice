=======================================================================================
 https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Type_basics#Function_Types
---------------------------------------------------------------------------------------
 Accessed at 2019-11-30T07:47
=======================================================================================



Lambda Calculus
~~~~~~~~~~~~~~~
Based on Lambda calculus. Instead of lambda, Haskell uses backslashes.
                          Instead of period (.), Haskell uses arrows (->).

Therefore, {\lambda x.x*x} would become {\x -> x*x}.

======================
 Examples
----------------------
 square = \x -> x * x
 f = \x y -> 2*x + y
======================



That Pesky IO Type
~~~~~~~~~~~~~~~~~~
IO Function type means that that function is an "Input-Output Action". These are not pure functions and should be isolated.



Explicit Type Declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~
Best practice to explicitly specify the types of all top level functions.
Written separately from the function definitions. Called type signatures.

===========================
 Examples
---------------------------
 square :: Num a => a -> a
 square x = x*x
===========================




____________________________________________________________________________________________________________________________________________________________________________________________________________
  square (x :: Int) = x*x

What is the type of square in this example? Make your guess then you can check it either by entering this code into a file and loading it into your interpreter or by asking for the type of the expression:

  Example:
    Prelude> :t (\(x :: Int) -> x*x)
````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
 Guess: Type of square is Int -> Int.
Actual: (\(x :: Int) -> x*x) :: Int -> Int

==========================================
 Side Note: -fglasgow-exts is deprecated.
==========================================




__________________________________________________________________________________________________________________________________________________
Figure out for yourself, and then verify the types of the following expressions, if they have a type. Also note if the expression is a type error:

  1. \x -> [x]
  2. \x y z -> (x,y:z:[])
  3. \x -> x + 5
  4. \x -> "hello, world"
  5. \x -> x 'a'
  6. \x -> x x
  7. \x -> x + x
``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````

=================================================================
 Guesses
-----------------------------------------------------------------
 CORRECT            1. a -> [a]
 CORRECT            2. a -> b -> b -> (a, [b])
 CORRECT            3. Num a => a -> a
 CORRECT            4. a -> [Char]
 WRONG              5. Error. No operator between {x} and {'a'}.
 SLIGHTLY WRONG     6. Error. No operator between both {x}s.
 CORRECT            7. Num a => a -> a
=================================================================

========================================================================
 Actual
------------------------------------------------------------------------
 1. \x -> [x] :: a -> [a]
 2. \x y z -> (x,y:z:[]) :: a1 -> a2 -> a2 -> (a1, [a2])
 3. \x -> x + 5 :: Num a => a -> a
 4. \x -> "hello, world" :: p -> [Char]
 5. \x -> x 'a' :: (Char -> t) -> t

 6. <interactive>:1:9: error:
        * Occurs check: cannot construct the infinite type: t ~ t -> t1
        * In the first argument of `x', namely `x'
          In the expression: x x
          In the expression: \ x -> x x
        * Relevant bindings include
            x :: t -> t1 (bound at <interactive>:1:2)

 7. \x -> x + x :: Num a => a -> a
========================================================================
