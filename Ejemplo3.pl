valor_max(X,Y,Z):-
    X>Y,Z is X.
valor_max(X,Y,Z):-
    X=<Y,Z is Y.

/*
 ?-valor_max(4,5,F),write(F),nl.
*/
