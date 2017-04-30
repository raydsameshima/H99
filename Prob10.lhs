Prob10.lhs

> module Prob10 where

> import Data.List (group)

Run-length encoding of a list.
Use the result of prob09.lhs to implement the so-called run-length encoding data compression method.
Consecutive duplicates of elements are encoded as lists (N, E) where N is the number of duplicates of the element E.

> encode :: Eq a => [a] -> [(Int, a)]
> encode = map encode' . group
>   where 
>     encode' xx@(x:_) = (length xx, x)

Or writing it pointfreestyle 
(Note that the type signature is essential here to avoid hitting 
  Monomorphism Restriction
);

  *Main Data.List> group "aaaabccaadeeee"
  ["aaaa","b","cc","aa","d","eeee"]
  *Main Data.List> map (\x -> (length x, head x)) it
  [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

> encode' :: Eq a => [a] -> [(Int, a)]
> encode' = map (\x -> (length x, head x)) . group
  
  *Prob09> :!hlint Prob10.lhs
  Prob10.lhs:27:18: Suggestion: Use &&&
  Found:
    \ x -> (length x, head x)
  Why not:
    length Control.Arrow.&&& head

> -- Using list comprehension.
> encode'' :: Eq a => [a] -> [(Int, a)]
> encode'' xs = [(length x, head x) | x <- group xs]
