prob07.lhs

Flatten a nested list structure.

We have to define a new data type, because lists in Haskell are homogeneous.

> data NestedList a = Elem a 
>                   | List [NestedList a]
> 
> myFlatten :: NestedList a -> [a]
> myFlatten (List [])     = []
> myFlatten (Elem x)      = [x]
> myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
> -- myFlatten (List [])     = []

Just a copy and paste of the solutions.

Simple implementation using 
  concatMap :: Foldable t => (a -> [b]) -> t a -> [b] 
is the following: 

> myFlatten' :: NestedList a -> [a]
> myFlatten' (Elem x) = [x]
> myFlatten' (List x) = concatMap myFlatten' x

If you know the Monad,

> myFlatten'' :: NestedList a -> [a]
> myFlatten'' (Elem x) = return x
> -- myFlatten'' (List x) = myFlatten'' =<< x
> myFlatten'' (List x) = x >>= myFlatten''

  flatten3 :: NestedList a -> [a]
  flatten3 (Elem x ) = [x]
  flatten3 (List xs) =  foldr (++) [] $ map flatten3 xs
