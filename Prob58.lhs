Prob58.lhs

> module Prob58 where

Generate-and-test paradigm

Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

> import Prob54(Tree(..))
> import Prob55(cbalTree)
> import Prob56(isSym)
>
> symCbalTrees :: Int -> [Tree Char]
> symCbalTrees n = filter isSym (cbalTree n)

  *Prob58> symCbalTrees 5
  [Branch 'x' (Branch 'x' Empty 
                          (Branch 'x' Empty Empty)
              ) 
              (Branch 'x' (Branch 'x' Empty Empty) 
                           Empty
              )
  ,Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
                          Empty
              ) 
              (Branch 'x' Empty 
                          (Branch 'x' Empty Empty)
              )
  ]
