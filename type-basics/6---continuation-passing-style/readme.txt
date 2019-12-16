===================================================================================================
 https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Type_basics#Continuation_Passing_Style
---------------------------------------------------------------------------------------------------
 Accessed at 2019-12-12T07:21
===================================================================================================

Idea behind CPS (Continuation Passing Style) is to pass a function argument detailing the next steps.
Little difficult from my one-line sentence. Refer to the link given above to understand it better.




__________________________________________________________________________________________________
Test whether the CPS-style fold mimics either of foldr and foldl. If not, where is the difference?
``````````````````````````````````````````````````````````````````````````````````````````````````
-- Helper function. Allows us to easily revert the order to perform operations on.
cfold' f z [] = z
cfold' f z (x:xs) = f x z (\y -> cfold' f y xs)

-- Actual function.
cfold f z l = cfold' (\x t g -> f x (g t)) z l



RELEVANT NOTES
~~~~~~~~~~~~~~
Won't work if tried in the Prelude module when you load GHCi directly because the second {cfold'} overwrites(?) the first one. This leads to non-exhaustive patterns.
Works if put in a separate module and compiling/running that module.

=========================================================================================
 Working for     cfold (/) 1 [1,2,3]
-----------------------------------------------------------------------------------------
 cfold' (\x t g -> (/) x (g t)) 1 [1,2,3]
 (\x t g -> (/) x (g t)) 1 1 (\y -> cfold' (\x t g -> (/) x (g t)) y [2,3])
 (/) 1 ((\y -> cfold' (\x t g -> (/) x (g t)) y [2,3]) 1)
 (/) 1 (cfold' (\x t g -> (/) x (g t)) 1 [2,3])
 (/) 1 ((\x t g -> (/) x (g t)) 2 1 (\y -> cfold' (\x t g -> (/) x (g t)) y [3]))
 (/) 1 ((/) 2 ((\y -> cfold' (\x t g -> (/) x (g t)) y [3]) 1))
 (/) 1 ((/) 2 (cfold' (\x t g -> (/) x (g t)) 1 [3]))
 (/) 1 ((/) 2 ((\x t g -> (/) x (g t)) 3 1 (\y -> cfold' (\x t g -> (/) x (g t)) y [])))
 (/) 1 ((/) 2 ((/) 3 ((\y -> cfold' (\x t g -> (/) x (g t)) y []) 1)))
 (/) 1 ((/) 2 ((/) 3 (cfold' (\x t g -> (/) x (g t)) 1 [])))
 (/) 1 ((/) 2 ((/) 3 1))
 (/) 1 ((/) 2 3)
 (/) 1 0.6666...
 1.5
=========================================================================================

==============================================================================
 Working for     foldl (/) 1 [1,2,3]
------------------------------------------------------------------------------
 (((/) 1 1) (foldl 1 [2,3]))
 (((/) ((/) 1 1) 2) (foldl 1 [3]))
 ((/) ((/) ((/) 1 1) 2) 3)             (((1/1)/2)/3) [Clearer representation]
 ((/) ((/) 1 2) 3)
 (/) 0.5 3
 0.1666...                             1/6 [Clearer representation]
==============================================================================



MY ANSWER
~~~~~~~~~
The CPS-style fold {cfold} mimics {foldr}. The difference between foldl and cfold is essentially the same as the difference between {foldr} and {foldl}. {foldl}-like behaviour can be obtained using a variation on {cfold'} (e.g. cfold' (\x t g -> g (x : t)) [] [1..10]).



ACTUAL ANSWER
~~~~~~~~~~~~~
It mimics neither. Behaviour closely resembles     {foldr}     but differs by using the initial value at the start, instead of the end.




______________________________________________________
Write map and filter using continuation passing style.
``````````````````````````````````````````````````````
cmap' f [] = []
cmap' f (x:xs) = f x (\y -> y : cmap' f xs)
cmap f l = cmap' (\x g -> g (f x)) l

cfilter' f c [] = []
cfilter' f c (x:xs) =
  if c x
  then
    f x (\y -> y : cfilter' f c xs)
  else
    cfilter' f c xs
cfilter c l = cfilter' (\x g -> g x) c l



=========================================
 ACTUAL ANSWER for     CPS-style map
-----------------------------------------
 map' :: (a -> [b] -> [b]) -> [a] -> [b]
 map' f [] = []
 map' f (x:xs) = f x  (map' f xs)

 map2 :: (a -> b) -> [a] -> [b]
 map2 f l = map' (\x y -> f x : y) l
-----------------------------------------
 Point Elimination

 map2 f = map' (\x y -> (:) (f x) y)
 map2 f = map' (\x -> (:) (f x))
 map2 f = map' (\x -> ((:) . f) x)
 map2 f = map' ((:) . f)
=========================================



=================================================================================
 ACTUAL ANSWER for     CPS-style filter
---------------------------------------------------------------------------------
 Needs to be checked.
---------------------------------------------------------------------------------
 filter' :: (a -> [b] -> [b]) -> [a] -> [b]
 filter' f [] = []
 filter' f (x:xs) = f x  (filter' f xs)

 filter2 :: (a -> Bool) -> [a] -> [a]
 filter2 f l = filter' (\x y -> if (f x) then x:y else y) l
---------------------------------------------------------------------------------
 Had the right idea earlier, but didn't know how to use the     else     clause.
=================================================================================



RELEVANT NOTES
~~~~~~~~~~~~~~
===========================================
 Function definitions for the normal {map}
-------------------------------------------
 map f [] = []
 map f (x:xs) = f x : map f xs
===========================================

==============================================
 Function definitions for the normal {filter}
----------------------------------------------
 filter cond [] = []
 filter cond (x:xs) =
   if cond x
   then
     x : Cps.filter cond xs
   else
     Cps.filter cond xs
==============================================
