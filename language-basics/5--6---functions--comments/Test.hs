module Test
    where

square x = x * x

signum x =
    if x < 0
      then -1
      else if x > 0
        then 1
        else 0

f x =
    case x of
      0 -> 1     -- 0 maps to 1
      1 -> 5     -- 1 maps to 5
      2 -> 2     -- 2 maps to 2
      _ -> -1    -- everything else maps to -1

{- =========================================================================================
    Can also write the function 'f' as
   -----------------------------------------------------------------------------------------
    f x = case x of
            { 0 -> 1 ; 1 -> 5 ; 2 -> 2 ; _ -> -1 }
   -----------------------------------------------------------------------------------------
    or in any other structure as long as the semicolons and braces are consistently placed.
   =========================================================================================
   
   ===============================================================================================================
    The function 'f' can be defined piece-wise as well.
   ---------------------------------------------------------------------------------------------------------------
    f 0 = 1
    f 1 = 1
    f 2 = 2
    f _ = -1
   ---------------------------------------------------------------------------------------------------------------
    Order is important. If { f _ = -1 } was placed first, then the function would return (-1) for every argument.
   =============================================================================================================== -}
   
roots a b c =
    let discr = sqrt (b*b - 4*a*c)
        twice_a = 2*a
    in ((-b + discr) / twice_a,
        (-b - discr) / twice_a)

non_infix_sum a b = (+) a b

infix_map func (x:xs) = func `map` (x:xs)
