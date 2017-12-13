
edge(a,b,10).
edge(b,c,10).

path(X,X,C) :- =(0,C).
path(X, Y, M) :- edge(Z, Y, P), C is M-P, path(X, Z, C).




grows([_| []]).
grows([A | [B |T]]) :- <(A, B), grows([B|T]).

sumrunner([],[]).
sumrunner([A|B], [X|[Y|Z]]) :- X is 0, Y is A+X , sumrunner(B, [Y|Z]). 
