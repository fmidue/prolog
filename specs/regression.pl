days(january,31).
days(february,28).
days(march,31).
days(april,30).
days(may,31).
days(june,31).
days(july,30).
days(august,31).
days(september,30).
days(october,31).
days(november,30).
days(december,31).

valid(date(day(Z),month(M),year(Y))) :-
  Z > 0, days(M,D), Z =< D, Y > 0.

summer(date(day(Z),month(M),year(Y))) :-
  valid(date(day(Z),month(M),year(Y))), Z> 21, M=june;
  valid(date(day(Z),month(M),year(Y))), M = july;
  valid(date(day(Z),month(M),year(Y))), M = august;
  valid(date(day(Z),month(M),year(Y))), Z <21, M =september.

summer2(date(day(X), month(Y), year(Z))) :-
  days(Y,W),
    ((X>20, X<W, Y=june) ;
    (X>0, X<W, Y=july) ;
    (X>0, X<W,Y=august) ;
    (X>0,X<22,Y=september)).

summerW(date(day(X), month(Y), year(Z))) :-
  days(Y,W),
    (X>20, X<W, Y=june) ;
    (X>0, X<W, Y=july) ;
    (X>0, X<W,Y=august) ;
    (X>0,X<22,Y=september).
