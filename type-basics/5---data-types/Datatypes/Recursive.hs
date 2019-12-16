module Datatypes.Recursive     -- 3: Recursive Datatypes
    where

data List a = Nil
            | Cons a (List a)

listLength :: List a -> Integer
listHead :: List a -> Prelude.Maybe a
listTail :: List a -> List a
listFoldr :: (a -> a -> a) -> a -> List a -> a
listFoldl :: (a -> a -> a) -> a -> List a -> a

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = Nothing
listHead (Cons x xs) = Prelude.Just x

listTail Nil = Nil
listTail (Cons x xs) = xs

listFoldr func init Nil = init
listFoldr func init (Cons x xs) = func x (listFoldr func init xs)

listFoldl func init Nil = init
listFoldl func init (Cons x xs) = func (listFoldl func init xs) x
