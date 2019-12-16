module Datatypes.Constructors     -- 2: Multiple Constructors
    where

data Tuple a b c d = Just a
                   | Pair a b
                   | Triple a b c
                   | Quadruple a b c d

tuple1 :: Tuple a b c d -> Maybe a
tuple2 :: Tuple a b c d -> Maybe b
tuple3 :: Tuple a b c d -> Maybe c
tuple4 :: Tuple a b c d -> Maybe d

tuple1 (Datatypes.Two.Just a) = Prelude.Just a
tuple1 (Pair a b) = Prelude.Just a
tuple1 (Triple a b c) = Prelude.Just a
tuple1 (Quadruple a b c d) = Prelude.Just a

tuple2 (Datatypes.Two.Just a) = Nothing
tuple2 (Pair a b) = Prelude.Just b
tuple2 (Triple a b c) = Prelude.Just b
tuple2 (Quadruple a b c d) = Prelude.Just b

tuple3 (Datatypes.Two.Just a) = Nothing
tuple3 (Pair a b) = Nothing
tuple3 (Triple a b c) = Prelude.Just c
tuple3 (Quadruple a b c d) = Prelude.Just c

tuple4 (Datatypes.Two.Just a) = Nothing
tuple4 (Pair a b) = Nothing
tuple4 (Triple a b c) = Nothing
tuple4 (Quadruple a b c d) = Prelude.Just d



data Either a b c d = First a
                    | Second (a, b)
                    | Third (a, b, c)
                    | Fourth (a, b, c, d)

getHaskellType :: Tuple a b c d -> Datatypes.Two.Either a b c d
getHaskellType (Datatypes.Two.Just a) = First a
getHaskellType (Pair a b) = Second (a, b)
getHaskellType (Triple a b c) = Third (a, b, c)
getHaskellType (Quadruple a b c d) = Fourth (a, b, c, d)
