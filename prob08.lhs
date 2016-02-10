prob08.lhs

Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. 
The order of the elements should not be changed.

> compress :: Eq a => [a] -> [a]
> compress [] = []
> compress (x:xs) = x : (compress (dropWhile (== x) xs))

This is also copy and paste.
It's nice to use dropWhile

  dropWhile :: (a -> Bool) -> [a] -> [a]
  dropWhile _ [] = []
  dropWhile p xs@(x:xs')
    | p x        = dropWhile p xs'
    | otherwise  = xs

 compress' [] = []
 compress' [x] = [x]
 compress' (x:y:ys) 
  | x == y    = x : (compress' ys)
  | otherwise = x : (compress' (y:ys)) 

This does not work correctly, but I can fix it:

> compress'' :: Eq a => [a] -> [a]
> compress'' [] = []
> compress'' [x] = [x]
> compress'' (x:y:ys)
>   | x == y    = compress'' (y:ys)
>   | otherwise = x : (compress'' (y:ys))
