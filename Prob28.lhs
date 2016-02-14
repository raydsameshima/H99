Prob28.lhs

> module Prob28 where

Sorting a list of lists according to length of sublists.
a) We suppose that a list contains that are lists themselves.
The objective is to sort the elements of this list according to their length.

> import Data.List (sort, group)

> lsort :: (Ord a) => [[a]] -> [[a]]
> lsort []     = []
> lsort (x:xs) = shorter ++ (x : longer)
>   where
>     shorter = lsort [y | y <- xs, not (isLongerThan_x y)]
>     longer  = lsort [y | y <- xs, isLongerThan_x y]
>     isLongerThan_x y
>       | length y >= length x = True
>       | length y < length x = False
>       | otherwise = error "lsort:" 

b) Again, we suppose that a list contains elements that are lists themselves.
But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lits with rare length are placed first, others with a more frequent length come later.

> frequency :: [[a]] -> [a] -> Int
> frequency xs x
>   = length $ filter ((== length x) . length) xs

> lfsort :: (Ord a) => [[a]] -> [[a]]
> lfsort [] = []
> lfsort lst@(x:xs) = shorter ++ (x : longer)
>   where
>     shorter = lfsort [y | y <- xs, not (isFreqThan_x y)]
>     longer  = lfsort [y | y <- xs, isFreqThan_x y]
>     isFreqThan_x y 
>       | frequency lst y >= frequency lst x = True
>       | frequency lst y < frequency lst x = False
>       | otherwise = error "lfsrt:"

  *Prob28> length $ filter ((==3) . length) ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
  2

