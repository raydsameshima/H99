Prob07.lhs

> module Prob07 where

Flatten a nested list structure.

  *Prob07> myFlatten $ List [ Elem 1
                            , List [ Elem 2
                                   , List [ Elem 3
                                          , Elem 4
                                          ]
                                   , Elem 5
                                   ]
                            ]
  [1,2,3,4,5]

We have to define a new data type, since lists in Haskell should be 
homogeneous.

> data NestedList a 
>   = Elem a 
>   | List [NestedList a]
>   deriving (Eq, Show)
> 
> nLst :: NestedList Int
> nLst 
>  = List [ Elem 1
>         , List [ Elem 2
>                , List [ Elem 3
>                       , Elem 4
>                       ]
>                , Elem 5
>                ]
>         ] 

> myFlatten :: NestedList a -> [a]
> myFlatten (Elem x)      = [x]
> myFlatten (List [])     = []
> -- myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
> -- myFlatten (List (x:xs)) = (++) (myFlatten x) (myFlatten (List xs))
> myFlatten (List (x:xs)) = ((++) . myFlatten) x (myFlatten (List xs))

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
> myFlatten'' (List x) = x >>= myFlatten''
> -- myFlatten'' (List x) = myFlatten'' =<< x

Using 
  foldr :: (a -> b -> b) -> b -> [a] -> b
we can reduce myFlatten into the following form.
(Note that, the universality of foldr is
  g = foldr f v
iff
  g     [] = v
  g (x:xs) = f x (g xs)
("A tutorial on the universality and expressiveness of fold (G. Hutton)))

So once we write our flatten function like
  myFlatten (List (x:xs)) = ((++) . myFlatten) x (myFlatten (List xs))
then we reach the follwoing implementation.

> flatten3 :: NestedList a -> [a]
> flatten3 (Elem x ) = [x]
> flatten3 (List xs) = foldr ((++) . flatten3) [] xs

By the way, a nested list is just a tree.
