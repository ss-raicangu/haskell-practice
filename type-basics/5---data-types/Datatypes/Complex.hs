module Datatypes.Complex     -- 4: Binary Trees
    where

data BinaryTree a = Leaf a
                  | Branch (BinaryTree a) a (BinaryTree a)

treeSize :: BinaryTree a -> Integer
elements :: BinaryTree a -> [a]

treeSize (Leaf x) = 1
treeSize (Branch left x right) = 1 + treeSize left + treeSize right

elements (Leaf a) = a : []
elements (Branch left a right) = elements left ++ [a] ++ elements right



treeFoldr :: (a -> b -> b) -> b -> BinaryTree a -> b
elements2 :: BinaryTree a -> [a]

treeFoldr func init (Leaf a) = func a init
treeFoldr func init (Branch left a right) = treeFoldr func (func a (treeFoldr func init right)) left

elements2 = treeFoldr (:) []



treeFoldl :: (b -> a -> b) -> b -> BinaryTree a -> b
elements3 :: BinaryTree a -> [a]

treeFoldl func init (Leaf a) = func init a
treeFoldl func init (Branch left a right) = treeFoldl func (func (treeFoldl func init left) a) right

elements3 tree = treeFoldl (\i a -> i ++ [a]) [] tree
