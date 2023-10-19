suma_neg([],Res):-
    !.
suma_neg([X|Y],Res):-
    X<0,
    number(Res),
    Aux is Res + X*X,
    suma_neg(Y,Aux).

suma_neg([X|Y],Res):-
    X<0,
    Aux is X*X,
    suma_neg(Y,Aux).

suma_neg([_|Y],Res):-
    suma_neg(Y,Res).
