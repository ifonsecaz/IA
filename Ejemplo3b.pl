% valor_max(i,i,o):
valor_max(X,Y,X):-
    X>Y.
valor_max(X,Y,Y):-
    X=<Y.

/*
 ?-valor_max(4,5,F),write(F),nl.
*/
