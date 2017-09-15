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
>     p rev (x:xs) (_:_:ys) = p (x:rev) xs ys -- (3)
>     p rev (_:xs) [_]      = rev == xs       -- (1)
>     p rev xs     []       = rev == xs       -- (0)

E.g.,

rotor
  p [] rotor rotor
  p r  otor  tor    (3)
  p or tor   r      (3)
    ==> or==or      (1)
    ==> True

boneanob
  p []   boneanob boneanob
  p b    oneanob  neanob   (3)
  p ob   neanob   anob     (3)
  p nob  eanob    ob       (3)
  p enob anob     []       (0)
    ==> enob /= anob
    ==> False

The key idea is the following; the lists is palindrome iff the first half
is the reversed of the last half.

In (3), we take an element from first xs ant two elements from second xs.
By consing this single element to rev, rev will be the (partial) reversed
list of the original xs.
So, at the final state, second list becomes either singleton (odd) or
empty (even).
If we meet singleton, first xs becomes the central element with the rests.
So we only need to compare rev and the rests (1).
If we meet empty, we can simply compare first half and last half.
