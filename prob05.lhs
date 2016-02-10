prob05.lhs

Reverse a list.

> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse (x:xs) = (myReverse xs) ++ [x] 

This definition is more waseful than the standard definition in Prelude.
The standard definition, found in the prelude, is concise but not very readble:

reverse = foldl (flip (:)) []

Using accumulator, we can dramatically reduce the order from O(n^2) to O(n)!

> myReverse' :: [a] -> [a]
> myReverse' lst = myReverse' lst []
>   where myReverse' []     tsl = tsl
>         myReverse' (x:xs) tsl = myReverse' xs (x:tsl)

This is so-called Bustall-Darlington transformation, see 4.1.5 of Algorithms: A Functional Programming Approach (Fethi Rabhi, Guy Lapalme).

