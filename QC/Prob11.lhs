Prob11.lhs

> module Prob11 where

Modefied run-length encoding.
Modify the result of prob10.lhs in such a way that if an element has no duplicates it is simply copied into the result list.
Only elements with duplicates are transferred as (N, E) lists.

To represent such the list as a Haskel (homogeneous) list, we have to make a new data structure:

> import Data.List (group)

> data ListItem a = Multiple Int a 
>                 | Single a
>                 deriving (Show)

The following is basically the same as in prob10.lhs:

> encode :: Eq a => [a] -> [(Int, a)]
> encode lst = map encode' (pack lst)
>   where encode' xx@(x:xs) = (length xx, x)

> pack :: Eq a => [a] -> [[a]]
> pack [] = []
> pack (x:xs) = (x:front) : pack rear
>   where front = takeWhile (== x) xs
>         rear  = dropWhile (== x) xs
>
> listItemizer :: (Int, a) -> ListItem a
> listItemizer (1, x) = Single x
> listItemizer (n, x) = Multiple n x
>
> modefiedEncode :: Eq a => [a] -> [ListItem a]
> modefiedEncode lst = map listItemizer (encode lst) 

Essentially the same way in the solution

data ListeItem a = Single a | Multiple Int a
  deriving (Show)

encodeModefied :: Eq a => [a] -> [ListItem a]
encodeModefied = map encodeHelper . encode
  where encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x

The ListItem definition contains 'deriving (Show)' so that we can get interactive output.

This problem could also be solved using a list comprehension:

> modifiedEncode xs = 
>   [y | x <- group xs, 
>        let y = if (length x) == 1
>                then Single (head x)
>                else Multiple (length x) (head x)
>   ]
