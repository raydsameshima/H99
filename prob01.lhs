prob01.lhs

Find the last element of a list.

> myLast :: [a] -> a
> myLast [] = error "the input list is empty"
> myLast [x] = x
> myLast (x:xs) = myLast xs
> -- myLast (_:xs) = myLast xs

Use the place holder:

  myLast (_:xs) = myLast xs

An interesting implementation from the solutions:

  curry' :: ((a, b) -> c) -> a -> b -> c
  curry' f x y = f (x, y)

  curry snd :: a -> c -> c

  foldl1 :: (a -> a -> a) -> [a] -> a

> myLast' :: [a] -> a
> -- myLast' = foldl1 $ curry snd
> myLast' = foldl1 (curry snd)

This implementation continuously takes "second" element.
So, this will fail when applied on empty list.
For singleton list, the following behavior is guaranteed by the definition of foldl1:
  
> foldl1' :: (a -> a -> a) -> [a] -> a
> foldl1' f [x]    = x
> foldl1' f (x:xs) = foldl f x xs

So, we get

  myLast' [1] = foldl1 (curry snd) [1]
              = 1

Another interesting implementation is the following:

> myLast'' :: [a] -> a
> myLast'' = foldr1 (const id)

  const :: a -> b -> a
  const id :: b -> a -> a

The same rule holds for singleton list, and for longer list,

  myLast'' [1,2,3] = foldr1 (const id) [1,2,3]
                   = (const id) 1 $ foldr1 (const id) [2,3]
                   = (const id) 1 $ (const id) 2 $ 3
                   = (const id) 1 $ 3
                   = 3
