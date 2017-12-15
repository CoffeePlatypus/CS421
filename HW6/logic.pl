
edge(a,b,10).
edge(b,c,10).

path(X,X,C) :- =(0,C).
path(X, Y, M) :- edge(Z, Y, P), C is M-P, path(X, Z, C).

path(X,X,C,[X]):- =(0,C).
path(X, Y, M, [Y|L]) :- edge(Z, Y, P), C is M-P, path(X, Z, C, L).

grows([_| []]).
grows([A | [B |T]]) :- <(A, B), grows([B|T]).

sumrunner([],[]).
sumrunner([A|B], [X|[Y|Z]]) :- X is 0, Y is A+X , sumrunner(B, [Y|Z]).

sumrunner(A,B) :- sumrunner(A,B,0).
sumrunner([A|B], [X|[Y|Z]], S) :- X is S, Y is X+A, sumrunner(B, [Y|Z], Y).


sumrunner([],[]).
sumrunner([_|[]],[0|[]]).
sumrunner([A|[B|C]], [A|[Y|Z]]):- Y is A+B, sumrunner(C,Z).
