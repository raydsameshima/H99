Prob17.lhs

> module Prob17 where

Split a list into two parts; the length of the first part is given.
Do not use any predefined predicates.

That is, reproduce
  splitAt :: Int -> [a] -> ([a], [a])

> split2tuple 
>   :: [a] -> Int -> ([a], [a])
> split2tuple xs n = (take n xs, drop n xs)

But these are using predefined predicates.

> split' 
>   :: [a] -> Int -> ([a],[a])
> split' lst       0 = ([], lst) 
> split' []        _ = ([], [])
> split' lst@(_:_) n = helper ([], lst) n
>   where
>     helper o@(first, second) 0 = o
>     helper   (first, (y:ys)) n = helper (first ++ [y], ys) (n-1)

Without using (++),

> split'' 
>   :: [a] -> Int -> ([a], [a])
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

> split''' 
>   :: [a] -> Int -> ([a], [a])
> split''' [] _ = ([], [])
> split''' (x:xs) n
>   | n > 0 = (x : (fst (split''' xs (n-1)))
>             ,snd (split''' xs (n-1))
>             )
>   | otherwise = (fst (split''' xs 0)
>                 ,x : (snd (split''' xs 0))
>                 )

This is also efficient.

Using two list comprehensions,   

> split4 :: [a] -> Int -> ([a],[a])
> split4 lst n = (fstn, rest)
>   where
>     fstn = [x | (x,i) <- zip lst [1..], i <= n]
>     rest = [x | (x,i) <- zip lst [1..], i >  n]
 
  *Prob17> split''' [1..10000] 5000
  (0.31 secs, 50,218,368 bytes)
  *Prob17> split4 [1..10000] 5000
  (0.30 secs, 48,101,912 bytes)
This is also compatible to the above imprementation.
