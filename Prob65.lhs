Prob64.lhs

> module Prob65 where

> import Data.Maybe

> import Prob54(Tree(..))

An alternative layout method is depicted in the illustration below:

  01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 
1                                            n
2                    k                                               u
3        c                       m                       p
4  a           e                                               q
5           d     g

Find out the rules and write the corresponding function.
Hing: On a given level, the horizontal distance between neighboring nodes is const.

Use the same conventions as in problem 64 and test your function in an appropriate way.

Here is the example tree from above illustration:

> tree65 
>   = Branch 'n'
>            (Branch 'k'
>                    (Branch 'c'
>                            (Branch 'a' Empty Empty)
>                            (Branch 'e'
>                                    (Branch 'd' Empty Empty)
>                                    (Branch 'g' Empty Empty)
>                            )
>                    )
>                    (Branch 'm' Empty Empty)
>            )
>            (Branch 'u'
>                    (Branch 'p'
>                            Empty
>                            (Branch 'q' Empty Empty)
>                    )
>                    Empty
>            )



