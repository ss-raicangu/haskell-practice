module Count
    where

import Data.Char

count1 bTest list = length (filter bTest list)
count2 bTest list = foldr (\x cnt -> if bTest x then cnt+1 else cnt) 0 list
