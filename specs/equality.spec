?- a = 1.
false.

?- X = a.
X = a.

?- X == a.
false.

?- a == b.
false.

?- f(a) == f(a).
true.

?- f(a) \= f(X).
false.

?- 3 =:= 5 - 2.
true.

?- 3 * 5 =:= 5 * 2 + 7 - 2.
true.

?- 3 * 5 =:= 5 * 2 + 7 - 1.
false.

?- 3 < 2.
false.

?- 3+8 < 2+10.
true.

?- 8 =< 100.
true.

?- 5+1 > 5.
true.

?- 5 >= 5, 7 >= 4.
true.
