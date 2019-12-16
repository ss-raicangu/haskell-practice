===================================================================================
 https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Type_basics#Data_Types
-----------------------------------------------------------------------------------
 Accessed at 2019-11-30T08:56
===================================================================================

Data structures, a.k.a. datatypes, are defined using the "data" keyword.




_______________________________________________________________________________________________________________________________________________________________________________________________________________
Write a data type declaration for Triple, a type which contains three elements, all of different types. Write functions tripleFst, tripleSnd and tripleThr to extract respectively the first, second and third.
```````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
data Triple a b c = Triple a b c
tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c




______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
Write a datatype Quadruple which holds four elements. However, the first two elements must be the same type and the last two elements must be the same type. Write a function firstTwo which returns a list containing the first two elements and a function lastTwo which returns a list containing the last two elements. Write type signatures for these functions.
``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
data Quadruple a b = Quadruple a a b b

firstTwo (Quadruple a b c d) = [a, b]
lastTwo (Quadruple a b c d) = [c, d]

firstTwo :: Quadruple a b -> [a]
lastTwo :: Quadruple a b -> [b]





RELEVANT NOTE
~~~~~~~~~~~~~
Need to define the Quadruple fully in the functions, e.g. firstTwo (Quadruple 1 2 'a' 'b'), for them to work. However, the type of the first two and last two must be the same since that was what was defined in the constructor.




_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
Write a datatype Tuple which can hold one, two, three or four elements, depending on the constructor (that is, there should be four constructors, one for each number of arguments). Also provide functions tuple1 through tuple4 which take a tuple and return Just the value in that position, or Nothing if the number is invalid (i.e., you ask for the tuple4 on a tuple holding only two elements).
`````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
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




____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
Based on our definition of Tuple from the previous exercise, write a function which takes a Tuple and returns either the value (if it's a one-tuple), a Haskell-pair (i.e., ('a',5)) if it's a two-tuple, a Haskell-triple if it's a three-tuple or a Haskell-quadruple if it's a four-tuple. Remember: it is not possible to write a function which return different types. You will need to use the Either type to represent this.
````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
data Either a b c d = First a
                    | Second (a, b)
                    | Third (a, b, c)
                    | Fourth (a, b, c, d)

getHaskellType :: Tuple a b c d -> Datatypes.Two.Either a b c d
getHaskellType (Datatypes.Two.Just a) = First a
getHaskellType (Pair a b) = Second (a, b)
getHaskellType (Triple a b c) = Third (a, b, c)
getHaskellType (Quadruple a b c d) = Fourth (a, b, c, d)



==================================================================================
 ACTUAL ANSWER
----------------------------------------------------------------------------------
 fromTuple :: Tuple a b c d -> Either (Either a (a,b)) (Either (a,b,c) (a,b,c,d))
 fromTuple (One   a      ) = Left  (Left  a        )
 fromTuple (Two   a b    ) = Left  (Right (a,b)    )
 fromTuple (Three a b c  ) = Right (Left  (a,b,c)  )
 fromTuple (Four  a b c d) = Right (Right (a,b,c,d))
==================================================================================




______________________________________________________________________________________________________________________________________________________________________________________________________
Write functions listHead, listTail, listFoldl and listFoldr which are equivalent to their Prelude twins, but function on our List datatype. Don't worry about exceptional conditions on the first two.
``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
listHead :: List a -> Prelude.Maybe a
listTail :: List a -> List a
listFoldr :: (a -> a -> a) -> a -> List a -> a
listFoldl :: (a -> a -> a) -> a -> List a -> a

listHead Nil = Nothing
listHead (Cons x xs) = Prelude.Just x

listTail Nil = Nil
listTail (Cons x xs) = xs

listFoldr func init Nil = init
listFoldr func init (Cons x xs) = func x (listFoldr func init xs)

listFoldl func init Nil = init
listFoldl func init (Cons x xs) = func (listFoldl func init xs) x



====================================================
 ACTUAL ANSWER for     listFoldl
----------------------------------------------------
 listFoldl f y Nil = y
 listFoldl f y (Cons x xs) = listFoldl f (f y x) xs
----------------------------------------------------
 Need to figure out why my version works though.
====================================================



RELEVANT NOTES (FOR listFolds)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Our list is     (Cons 1 (Cons 2 (Cons 4 (Cons 66 Nil))))

listFoldr (-) 1 (Cons 1 (Cons 2 (Cons 4 (Cons 66 Nil))))
  (-) 1 (listFoldr (-) 1 (Cons 2 (Cons 4 (Cons 66 Nil))))
  (-) 1 ((-) 2 (listFoldr (-) 1 (Cons 4 (Cons 66 Nil))))
  (-) 1 ((-) 2 ((-) 4 (listFoldr (-) 1 (Cons 66 Nil))))
  (-) 1 ((-) 2 ((-) 4 ((-) 66 (listFoldr (-) 1 Nil))))
  (-) 1 ((-) 2 ((-) 4 ((-) 66 1)))
  (-) 1 ((-) 2 ((-) 4 65))
  (-) 1 ((-) 2 -61))
  (-) 1 63
  -62

listFoldl (-) 1 (Cons 1 (Cons 2 (Cons 4 (Cons 66 Nil))))
  (-) ((-) 1 1) (listFoldl (-) 1 (Cons 2 (Cons 4 (Cons 66 Nil))))
  (-) 0 ((-) ((-) 1 2) (listFoldl (-) 2 (Cons 4 (Cons 66 Nil))))
  (-) 0 ((-) -1 ((-) ((-) 2 4) (listFoldl (-) 4 (Cons 66 Nil))))
  (-) 0 ((-) -1 ((-) -2 ((-) ((-) 4 66) (listFoldl (-) 66 Nil))))
  (-) 0 ((-) -1 ((-) -2 ((-) -62 66)))


foldl (-) 1 [1, 2, 4, 66]
  (1 - (foldl (-) 1 [2, 4, 66]))
  ((1 - 2) - (foldl (-) 1 [4, 66]))
  (((1 - 2) - 4) - (foldl (-) 1 [66]))
  ((((1 - 2) - 4) - 66) - (foldl (-) 1 []))
  ((((1 - 2) - 4) - 66) - 1)
  (((-1 - 4) - 66) - 1)
  ((-5 - 66) - 1)
  (-71 - 1)
  -72

listFoldl (-) 1 (Cons 1 (Cons 2 (Cons 4 (Cons 66 Nil))))
  (-) (listFoldl (-) 1 (Cons 2 (Cons 4 (Cons 66 Nil)))) 1
  (-) ((-) (listFoldl (-) 1 (Cons 4 (Cons 66 Nil))) 2) 1
  (-) ((-) ((-) (listFoldl (-) 1 (Cons 66 Nil)) 4) 2) 1
  (-) ((-) ((-) ((-) (listFoldl (-) 1 Nil) 66) 4) 2) 1
  (-) ((-) ((-) ((-) 1 66) 4) 2) 1
  (-) ((-) ((-) -65 4) 2) 1
  (-) ((-) -69 2) 1
  (-) -71 1
  -72




____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
Write a function elements which returns the elements in a BinaryTree in a bottom-up, left-to-right manner (i.e., the first element returned is the left-most leaf, followed by its parent's value, followed by the other child's value, and so on). The result type should be a normal Haskell list.
````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
elements :: BinaryTree a -> [a]

elements (Leaf a) = a : []
elements (Branch left a right) = elements left ++ [a] ++ elements right




___________________________________________________________________________________________________________________________________________________________________________________________
Write a foldr function treeFoldr for BinaryTrees and rewrite elements in terms of it (call the new one elements2). The type of treeFoldr should be (a -> b -> b) -> b -> BinaryTree a -> b.
```````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
treeFoldr :: (a -> b -> b) -> b -> BinaryTree a -> b
elements2 :: BinaryTree a -> [a]

treeFoldr func init (Leaf a) = func a init
treeFoldr func init (Branch left a right) = treeFoldr func (func a (treeFoldr func init right)) left

elements2 = treeFoldr (:) []



RELEVANT NOTES
~~~~~~~~~~~~~~
=======================     ===============================
      Binary Tree            treeFoldr Working
-----------------------     -------------------------------
           1                      Function: (+)
           |                 Initial Value:  1
     +-----+-----+                   Order:  4 2 5 1 6 3 7
     |           |
     2           3           Using the function in-fix 
     |           |          -------------------------------
 +---+---+   +---+---+       Cut short here...
 |       |   |       |       Looked at solution.
 4       5   6       7      ===============================
=======================

Long-hand form of writing the 'elements2' function: elements2 tree = treeFoldr (\a b -> a : b) [] tree




__________________________________________________________________________________________________________________
Write a foldl function treeFoldl for BinaryTrees and rewrite elements in terms of it (call the new one elements3).
``````````````````````````````````````````````````````````````````````````````````````````````````````````````````
treeFoldl :: (b -> a -> b) -> b -> BinaryTree a -> b
elements3 :: BinaryTree a -> [a]

treeFoldl func init (Leaf a) = func init a
treeFoldl func init (Branch left a right) = treeFoldl func (func (treeFoldl func init left) a) right

elements3 tree = treeFoldl (\i a -> i ++ [a]) [] tree



RELEVANT NOTES
~~~~~~~~~~~~~~
Need to define the lambda function instead of just (++) in the elements3 function because Haskell will expect the tree type to be 'BinaryTree [a]' otherwise.
Copied both treeFold solutions from the tutorial. Couldn't solve them on my own.





NOTES FOR ENUMERATION
~~~~~~~~~~~~~~~~~~~~~
Can define an enumeration as illustrated below.

data Color = Red
           | Orange
           | Yellow
           | Green
           | Blue
           | Purple
           | White
           | Black
           | Custom Int Int Int -- R G B components



And give meaning to an enumeration as so:

colorToRGB Red    = (255,0,0)
colorToRGB Orange = (255,128,0)
colorToRGB Yellow = (255,255,0)
colorToRGB Green  = (0,255,0)
colorToRGB Blue   = (0,0,255)
colorToRGB Purple = (255,0,255)
colorToRGB White  = (255,255,255)
colorToRGB Black  = (0,0,0)
colorToRGB (Custom r g b) = (r,g,b)





NOTES FOR THE UNIT TYPE
~~~~~~~~~~~~~~~~~~~~~~~
Defined in the Prelude as such: data () = ()
Equivalent to the 'void' type in other languages.
