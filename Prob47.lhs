Prob47.lhs

> module Prob47 where

Truth tables for logical expressions (2).

Continue problem P46 by defining and/2, or/2, etc as being operators. 
This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). 
Define operator precedence as usual; i.e. as in Java.

> import Prob46

Nothing to do.

  *Prob47> table (\a b -> (and' a (or' a b)))
  True  True  True 
  True  False True 
  False True  False
  False False False
  *Prob47> table (\a b -> (a `and'` (a `or'` b)))
  True  True  True 
  True  False True 
  False True  False
  False False False
