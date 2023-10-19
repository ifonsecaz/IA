:- dynamic polinomio/3.

lista(A,B,I,[X]):-
    B=:=I,
    X is A,
    !.

lista(A,B,I,[X|Y]):-
    J is I + 1,
    X is 0,
    lista(A,B,J,Y).


degree(Id,G):-
    polinomio(B,_,Id),
    G is B.

calculaGrado([],_,_):-
    !.

calculaGrado([X|Y],I,G):-
    X =\= 0,
    G is I,
    J is I + 1,
    calculaGrado(Y,J,G).

calculaGrado([_|Y],I,G):-
    J is I + 1,
    calculaGrado(Y,J,G).

sumaR([X1],[X2],I,Grado3,[XR]):-
    I =:= Grado3,
    XR is X1 + X2,
    !.

sumaR([],Pol,_,_,Pol):-
!.

sumaR(Pol,[],_,_,Pol):-
!.

sumaR([X1|Y1],[X2|Y2],I,Grado3,[XR|YR]):-
    XR is X1 + X2,
    J is I + 1,
    sumaR(Y1,Y2,J,Grado3,YR).

mayorGrado(G1,G2,G1):-
    G1 > G2,
    !.

mayorGrado(_,G2,G2).

suma:-
    write("Dame el nombre del primer polinomio"),
    nl,
    read(A),
    write("Dame el segundo polinomio"),
    nl,
    read(B),
    write("Dame el nombre para guardar el resultado"),
    nl,
    read(C),
    polinomio(Grado1,Lista1,A),
    polinomio(Grado2,Lista2,B),
    mayorGrado(Grado1,Grado2,Grado3),
    sumaR(Lista1,Lista2,0,Grado3,Lista3),
    write("El resultado es: "),
    write(Lista3),
    nl,
    write("El grado del polinomio es: "),
    write(Grado3),
    nl,
    asserta(polinomio(Grado3,Lista3,C)).

constructor:-
    write("Dame el coeficiente"),
    nl,
    read(A),
    write("Dame el exponente"),
    nl,
    read(B),
    write("Dame el identificador"),
    nl,
    read(Id),
    lista(A,B,0,Coef),
    write(Coef),
    asserta(polinomio(B,Coef,Id)).








