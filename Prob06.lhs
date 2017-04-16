Prob06.lhs

> module Prob06 where

> import Prob05 (myReverse')

Find out whether a list is a palindrome.
A palindrome can be read forward or backward; e.d. (xamax).

> isPalindrome :: Eq a => [a] -> Bool
> isPalindrome lst = (myReverse' lst == lst)

myReverse is from prob05.lhs

A nicer implementation is the following, it only flips the half:

> isPalindrome' xs = p [] xs xs
>   where 
>     p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
>     p rev (x:xs) [_]      = rev == xs
>     p rev xs     []       = rev == xs

E.g.,

rotor
  p [] rotor rotor
  p r otor tor
  p or tor r
    ==> r==r = True

boneanob
  p [] boneanob boneanob
  p b oneanob neanob
  p ob neanob anob
  p nob eanob ob
  p enob anob []
    ==> enob /= anob = False
