Prob08.lhs

> module Prob08 where

> import Data.List (group)

Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. 
The order of the elements should not be changed.

> compress :: (Eq a) => [a] -> [a]
> compress []     = []
> compress (x:xs) = x : (compress (dropWhile (== x) xs))

This is also copy and paste.
It's nice to use dropWhile

  dropWhile :: (a -> Bool) -> [a] -> [a]
  dropWhile _ [] = []
  dropWhile p xs@(x:xs')
    | p x        = dropWhile p xs'
    | otherwise  = xs

> compress'' :: (Eq a) => [a] -> [a]
> compress'' []  = []
> compress'' [x] = [x]
> compress'' (x:yy@(y:_))
>   | x == y    = compress'' yy
>   | otherwise = x : (compress'' yy)

  *Prob08> group "aaaabccaadeeee"
  ["aaaa","b","cc","aa","d","eeee"]
  *Prob08> map head it
  "abcade"

> compress''' :: (Eq a) => [a] -> [a]
> compress''' = map head . group
