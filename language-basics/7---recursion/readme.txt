======================================================================================
 https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Language_basics#Recursion
--------------------------------------------------------------------------------------
 Accessed at 2019-11-28T06:51
======================================================================================

Needed to use the fully qualified name for the exponent function as Prelude has an exponent function.




___________________________________________________________________________________________________
The fibonacci sequence is defined by:

       | 1                     n = 1 or n = 2
F_n = -|
       | F_{n-2} + F_{n-1}     otherwise

Write a recursive function fib that takes a positive integer n as a parameter and calculates F_{n}.
```````````````````````````````````````````````````````````````````````````````````````````````````
fib 1 = 1
fib 2 = 1
fib n = fib (n-2) + fib (n-1)




__________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
Define a recursive function mult that takes two positive integers a and b and returns a*b, but only uses addition (i.e., no fair just using multiplication). Begin by making a mathematical definition in the style of the previous exercise and the rest of this section.
``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````

INCORRECT ANSWER (Forgot to include the case where 'b' is negative)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

=======================================
 Mathematical Definition
---------------------------------------
           | 0                if b = 0
 F(a,b) = -| a                if b = 1
           | a + F(a,b-1)     if b > 1
=======================================

mult _ 0 = 0
mult a 1 = a
mult a b = a + (mult a (b-1))


CORRECT ANSWER | Accessed at 2019-11-30T02:28 from https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Language_basics/Solutions#Recursion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

========================================
 Mathematical Definition
----------------------------------------
          | -(a * (-b))       b < 0
          | 0                 b = 0
 a * b = -|
          | a                 b = 1
          | a + a * (b-1)     otherwise
========================================

mult a 0 = 0
mult a 1 = a
mult a b =
    if b < 0
        then 0 - mult a (-b)
        else a + mult a (b-1)




_________________________________________________________________________________________
Define a recursive function my_map that behaves identically to the standard function map.
`````````````````````````````````````````````````````````````````````````````````````````
my_map _ [] = []
my_map func (x:xs) = func x : my_map func xs

