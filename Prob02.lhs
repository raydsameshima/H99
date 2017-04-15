Prob02.lhs

> module Prob02 where

Find the last but one element of a list.

Pattern match:

> myButLast :: [a] -> a
> myButLast [x,_]  = x
> myButLast (_:xs) = myButLast xs
> myButLast _      = error "need 2 or more elements"

  *Main> head . tail $ "aiueo"
  'i'

It is always good to use built in functions, but keep in mind that it might fail due to the emptylist.

> myButLast' :: [a] -> a
> myButLast' = head . tail . reverse

