module Datatypes.Simple     -- 1
    where



data Pair a b = Pair a b
pairFst (Pair x y) = x
pairSnd (Pair x y) = y



data Triple a b c = Triple a b c
tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c



data Quadruple a b = Quadruple a a b b

firstTwo (Quadruple a b c d) = [a, b]
lastTwo (Quadruple a b c d) = [c, d]

firstTwo :: Quadruple a b -> [a]
lastTwo :: Quadruple a b -> [b]
