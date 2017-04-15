prob10.lhs

Run-length encoding of a list.
Use the result of prob09.lhs to implement the so-called run-length encoding data compression method.
Consecutive duplicates of elements are encoded as lists (N, E) where N is the number of duplicates of the element E.

> import Data.List (group)

> encode :: Eq a => [a] -> [(Int, a)]
> encode = map encode' . pack 
>   where 
>     encode' xx@(x:_) = (length xx, x)
>
> pack :: Eq a => [a] -> [[a]]
> pack [] = []
> pack (x:xs) = (x:front) : pack rear
>   where front = takeWhile (== x) xs
>         rear  = dropWhile (== x) xs

pack is in prob09.lhs

Or writing it pointfreestyle (Note that the type signature is essential here to avoid hitting the Monomorphism Restriction):

  *Main Data.List> group "aaaabccaadeeee"
  ["aaaa","b","cc","aa","d","eeee"]
  *Main Data.List> map (\x -> (length x, head x)) it
  [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

> pointfreeEncode :: Eq a => [a] -> [(Int, a)]
> pointfreeEncode = map (\x -> (length x, head x)) . group

