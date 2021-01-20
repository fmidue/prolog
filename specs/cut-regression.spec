?- not(not(g(u,[v,w,x],[v,u,w,u,x]))).
true.

?- not(not(g(u(v),[a(b)],[a(b)]))).
true.

?- xxxList(17,[],L),xyzFunc(xxx,L,R),!,not(not(g(xxx,L,R))).
L = [17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1],
R = [17, xxx, 16, xxx, 15, xxx, 14, xxx, 13, xxx, 12, xxx, 11, xxx, 10, xxx, 9, xxx, 8, xxx, 7, xxx, 6, xxx, 5, xxx, 4, xxx, 3, xxx, 2, xxx, 1].

?- not(not(g([],[],[]))).
true.
