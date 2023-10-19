:- dynamic polinomials/3.

creador(Coef, GradoCoef,I, [X]):-
    I =:= GradoCoef,
    X is Coef,
    !.

creador(Coef, GradoCoef,I, [X|Y]):-
    J is I + 1,
    X is 0,
    creador(Coef, GradoCoef, J,Y).

/*degree([], -1).
degree([0], -1).
degree([_|T],N):-
    degree(T,X),
    N is X + 1.
*/

calculaGrado([],_,Fin,Fin):-
    !.

calculaGrado([X|Y],I,_,Fin):-
    X =\= 0,
    G2 is I,
    J is I + 1,
    calculaGrado(Y,J,G2,Fin).

calculaGrado([_|Y],I,G,Fin):-
    J is I + 1,
    calculaGrado(Y,J,G,Fin).


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

negativo([X],[-X]):-
    !.

negativo([X|Y], [N|Z]):-
    N is -1 * X,
    negativo(Y,Z).


restaR([X1],[X2],I,Grado3,[XR]):-
    I =:= Grado3,
    XR is X1 - X2,
    !.

restaR([],Pol,_,_,NPol):-
    negativo(Pol, NPol).

restaR(Pol,[],_,_,Pol):-
    !.

restaR([X1|Y1],[X2|Y2],I,Grado3,[XR|YR]):-
    XR is X1 - X2,
    J is I + 1,
    restaR(Y1,Y2,J,Grado3,YR).


derivaR([X],Grado,I,ValAnt,[XR]):-
    I =:= Grado,
    XR is 0,
    ValAnt is X*I.

derivaR([X|Y],Grado,I,ValAnt,[XR|YR]):-
    I < Grado,
    J is I + 1,
    derivaR(Y,Grado,J,ValAct,YR),
    ValAnt is X*I,
    XR is ValAct.

mayorGrado(G1,G2,G1):-
    G1 > G2,
    !.

mayorGrado(_,G2,G2).

resta:-
    write("Dame el nombre del primer polinomio"),
    nl,
    read(A),
    write("Dame el nombre del segundo polinomio"),
    nl,
    read(B),
    write("Dame el nombre para guardar el resultado"),
    nl,
    read(C),
    polinomials(Grado1,Lista1,A),
    polinomials(Grado2,Lista2,B),
    mayorGrado(Grado1,Grado2, Grado3),
    restaR(Lista1,Lista2,0,Grado3,Lista3),
    write("El resultado es: "),
    write(Lista3),
    nl,
    calculaGrado(Lista3,0,_,Grad),
    write("El grado del polinomio es: "),
    write(Grad),
    nl,
    asserta(polinomials(Grad,Lista3,C)).

suma:-
    write("Dame el nombre del primer polinomio"),
    nl,
    read(A),
    write("Dame el nombre del segundo polinomio"),
    nl,
    read(B),
    write("Dame el nombre para guardar el resultado"),
    nl,
    read(C),
    polinomials(Grado1,Lista1,A),
    polinomials(Grado2,Lista2,B),
    mayorGrado(Grado1,Grado2, Grado3),
    sumaR(Lista1,Lista2,0,Grado3,Lista3),
    write("El resultado es: "),
    write(Lista3),
    nl,
    write("El grado del polinomio es: "),
    calculaGrado(Lista3,0,_,Grad),
    write(Grad),
    nl,
    asserta(polinomials(Grad,Lista3,C)).

deriva:-
    write("Dame el nombre del polinomio"),
    nl,
    read(A),
    write("Dame el nombre para guardar el polinomio"),
    nl,
    read(B),
    polinomials(Grado1,Lista1,A),
    derivaR(Lista1,Grado1,0,_,Lista2),
    write("El resultado es"),
    nl,
    write(Lista2),
    calculaGrado(Lista2,0,_,Grado2),
    write(Grado2),
    asserta(polinomials(Grado2,Lista2,B)).


constructor:-
    write("Dame el coeficiente"),
    nl,
    read(Coef),
    write("Dame el grado"),
    nl,
    read(GradoCoef),
    write("Dame el nombre del polinomio"),
    nl,
    read(Nom),
    nl,
    creador(Coef, GradoCoef, 0, Coefs), %Coefs=[0,0,0,0,0,0,0]
    write(Coefs),
    nl,
    calculaGrado(Coefs,0,_,G),
    write("Grado "),
    write(G),
    asserta( polinomials(GradoCoef,Coefs, Nom )).












