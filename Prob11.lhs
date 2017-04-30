Prob11.lhs

> module Prob11 where

Modefied run-length encoding.
Modify the result of prob10.lhs in such a way that if an element has no duplicates it is simply copied into the result list.
Only elements with duplicates are transferred as (N, E) lists.

To represent such the list as a Haskel (homogeneous) list, we have to make a new data structure:

> import Data.List (group)
> import Prob10 (encode)

> data ListItem a 
>   = Multiple Int a -- more than twice
>   | Single a
>   deriving (Show, Eq)

> listItemizer 
>   :: (Int, a) -> ListItem a
> listItemizer (1, x) = Single x
> listItemizer (n, x) = Multiple n x
>
> modifiedEncode 
>   :: Eq a => 
>      [a] -> [ListItem a]
> modifiedEncode = map listItemizer . encode

Essentially the same way in the solution

The ListItem definition contains 'deriving (Show)' so that we can get interactive output.

This problem could also be solved using list comprehension:

> modifiedEncode' 
>   :: Eq a => 
>      [a] -> [ListItem a]
> modifiedEncode' xs = 
>   [y | x <- group xs, 
>        let y = if length x == 1
>                  then Single (head x)
>                  else Multiple (length x) (head x)
>   ]
>
> modifiedEncode'' 
>   :: Eq a => 
>      [a] -> [ListItem a]
> modifiedEncode'' xs = [helper x | x <- group xs]
>   where
>     helper ts@(t:_) 
>       = if length ts == 1
>           then
>             Single t
>           else
>             Multiple (length ts) t
