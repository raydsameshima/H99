Prob54.lhs

> module Prob54 where

Check whether a given term represents a binary tree.

> data Tree a
>   = Empty
>   | Branch a (Tree a) (Tree a)
>   deriving (Show, Eq)

> tree1 
>   = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
>                            (Branch 'e' Empty Empty))
>                (Branch 'c' Empty
>                            (Branch 'f' (Branch 'g' Empty Empty)
>                                        Empty))
>
> -- leaf :: a -> Tree a
> leaf x = Branch x Empty Empty
>
> tree1'
>   = Branch 'a' (Branch 'b' (leaf 'd')
>                            (leaf 'e'))
>                (Branch 'c' Empty
>                            (Branch 'f' (leaf 'g')
>                                        Empty))

  *Prob54> :t tree1
  tree1 :: Tree Char
  *Prob54> :t tree1'
  tree1' :: Tree Char
  *Prob54> tree1 == tree1'
  True

> tree2 :: Tree Char
> tree2 = Branch 'a' Empty Empty

> tree3 :: Tree a
> tree3 = Empty

> tree4 :: Tree Int
> tree4 = Branch 1 (Branch 2 Empty
>                            (Branch 4 Empty Empty))
>                  (Branch 2 Empty Empty)
