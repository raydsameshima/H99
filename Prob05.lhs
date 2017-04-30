Prob05.lhs

> module Prob05 where

Reverse a list.

> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse (x:xs) = (myReverse xs) ++ [x] 

This definition is more wasteful than the standard definition in Prelude.
The standard definition, found in the prelude, is concise but not very readable:

  reverse = foldl (flip (:)) []

E.q.,

  reverse [1,2,3] 
    = foldl (flip (:)) [] [1,2,3]
    = foldl (flip (:)) 1:[] [2,3]
    = foldl (flip (:)) 2:1:[] [3]
    = foldl (flip (:)) 3:2:1:[] []
    = [3,2,1]

Using accumulator, we can dramatically reduce the order from O(n^2) to O(n)!

> myReverse' :: [a] -> [a]
> myReverse' lst = myReverse' lst []
>   where 
> --  myReverse' []     tsl = tsl
> --  myReverse' (x:xs) tsl = myReverse' xs (x:tsl)
>     myReverse' xs tsl = foldl (flip (:)) tsl xs

This is so-called Bustall-Darlington transformation, see 4.1.5 of Algorithms: A Functional Programming Approach (Fethi Rabhi, Guy Lapalme).

