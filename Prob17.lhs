Prob17.lhs

> module Prob17 where

Split a list into two parts; the length of the first part is given.
Do not use any predefined predicates.

> split :: [a] -> Int -> [[a]]
> split xs 0 = [xs]
> split [] _ = error "OVER"
> split xs n = (take n xs) : (drop n xs) : []

More simply, to tuple:

> split2tuple :: [a] -> Int -> ([a], [a])
> split2tuple xs n = (take n xs, drop n xs)

But these are using predefined predicates.

> split' :: [a] -> Int -> ([a],[a])
> split' lst        0 = ([], lst) 
> split' []         _ = ([], [])
> split' lst@(x:xs) n = helper ([], lst) n
>   where
>     helper (first, second) 0 = (first, second)
>     helper (first, (y:ys)) n = helper (first ++ [y], ys) (n-1)

Without using (++),

> split'' :: [a] -> Int -> ([a], [a])
> split'' []       _ = ([], [])
> split'' l@(x:xs) n
>   | n > 0          = (x : ys, zs)
>   | otherwise      = ([], l)
>   where
>     (ys,zs) = split'' xs (n-1)

  *Prob17> split' [1..1000] 500
  (0.03 secs, 13,054,408 bytes)
  *Prob17> split'' [1..1000] 500
  (0.03 secs, 6,765,496 bytes)

A recursive solution constructing the 2-tuple:

> split''' :: [a] -> Int -> ([a], [a])
> split''' [] _ = ([], [])
> split''' (x:xs) n
>   | n > 0 = (x : (fst (split''' xs (n-1)))
>             ,snd (split''' xs (n-1))
>             )
>   | otherwise = (fst (split''' xs 0)
>                 ,x : (snd (split''' xs 0))
>                 )

This is also efficient.
