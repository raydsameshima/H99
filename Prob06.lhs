Prob06.lhs

> module Prob06 where

> import Prob05 (myReverse')

Find out whether a list is a palindrome.
A palindrome can be read forward or backward; e.g. (xamax).

> isPalindrome :: Eq a => [a] -> Bool
> isPalindrome lst = myReverse' lst == lst

myReverse is from prob05.lhs

A nicer implementation is the following, it only flips the half:

> isPalindrome' xs = p [] xs xs
>   where 
>     p rev (x:xs) (_:_:ys) = p (x:rev) xs ys -- 3
>     p rev (x:xs) [_]      = rev == xs       -- 1
>     p rev xs     []       = rev == xs       -- 0

E.g.,

rotor
  p [] rotor rotor
  p r  otor  tor    (3)
  p or tor   r      (3)
    ==> or==or      (2)
    ==> True

boneanob
  p []   boneanob boneanob
  p b    oneanob  neanob
  p ob   neanob   anob
  p nob  eanob    ob
  p enob anob     []
    ==> enob /= anob = False
