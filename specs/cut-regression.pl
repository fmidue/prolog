g(_, [], []).
g(_, [X], [X]) :- !.
g(Element, [Head, Next | TailList], [Head, Element | MergedList]) :- g(Element, [Next | TailList], MergedList).

xxxList(0,Xs,Xs).
xxxList(N,Xs,[N|L]) :- N>0, N1 is N-1, xxxList(N1,Xs,L).

xyzFunc(_,[],[]).
xyzFunc(_,[Y],[Y]).
xyzFunc(X,[Y,Z|Zs],[Y,X|Us]) :- xyzFunc(X,[Z|Zs],Us).
