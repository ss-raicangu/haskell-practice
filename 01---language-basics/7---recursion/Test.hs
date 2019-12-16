module Test
    where

factorial 1 = 1
factorial n = n * factorial (n - 1)

exponent a 1 = a
exponent a b = a * Test.exponent a (b-1)

my_length [] = 0
my_length (x:xs) = 1 + my_length xs

my_filter p [] = []
my_filter p (x:xs) =
    if p x
      then x : my_filter p xs
      else my_filter p xs

fib 1 = 1
fib 2 = 1
fib n = fib (n-2) + fib (n-1)

-- Proper solution. In my case, I forgot to define the case for 'b' being negative.
mult a 0 = 0
mult a 1 = a
mult a b = 
    if b < 0
        then 0 - mult a (-b)
        else a + mult a (b-1)

-- My solution. It was incomplete.
-- mult _ 0 = 0
-- mult a 1 = a
-- mult a b = a + (mult a (b-1))

my_map _ [] = []
my_map func (x:xs) = func x : my_map func xs
