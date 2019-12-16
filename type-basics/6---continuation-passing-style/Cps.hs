module Cps
  where

-- Continuation Passing Style fold.
-- Helper function. Allows us to easily revert the order to perform operations on.
cfold' f z [] = z
cfold' f z (x:xs) = f x z (\y -> cfold' f y xs)
-- Actual function.
cfold f z l = cfold' (\x t g -> f x (g t)) z l

-- Continuation Passing Style map.
cmap' f [] = []
cmap' f (x:xs) = f x (\y -> y : cmap' f xs)
cmap f l = cmap' (\x g -> g (f x)) l

-- Continuation Passing Style filter. To be honest, this is more of a hack.
cfilter' f c [] = []
cfilter' f c (x:xs) =
  if c x
  then
    f x (\y -> y : cfilter' f c xs)
  else
    cfilter' f c xs
cfilter c l = cfilter' (\x g -> g x) c l
