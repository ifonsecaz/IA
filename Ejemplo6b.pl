% gusta(i,i):
gusta(jorge,X):-
    carne(X).
gusta(beatriz,X):-
    not(X==higado),
    carne(X).

carne(pancita).
carne(higado).
carne(filete).
