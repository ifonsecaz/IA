:-dynamic pobInicial/1.
:-dynamic pobFinal/1.
:-dynamic combinaciones/1.


equipos(1,alaves).
equipos(2,athletic).
equipos(3,atleticoMadrid).
equipos(4,barcelona).
equipos(5,realBetis).
equipos(6,cadiz).
equipos(7,celtaVigo).
equipos(8,elche).
equipos(9,espanyol).
equipos(10,getafe).
equipos(11,granada).
equipos(12,levante).
equipos(13,mallorca).
equipos(14,osasuna).
equipos(15,realMadrid).
equipos(16,realSociedad).
equipos(17,sevilla).
equipos(18,valencia).
equipos(19,villarreal).
equipos(20,vallecano).

numPartidos([[1,0],[2,0],[3,0],[4,0],[5,0],[6,0],[7,0],[8,0],[9,0],[10,0],[11,0],[12,0],[13,0],[14,0],[15,0],[16,0],[17,0],[18,0],[19,0],[20,0]]).

visitante(1,visitante).
visitante(0,local).

generaCombinaciones(Cont,Ac,Ac):-
    Cont>20,!.

generaCombinaciones(Cont,Ac,Res):-
    Cont=<20,
    Aux is Cont+1,
    generaComb2(Cont,Aux,Ac,Ac2),
    generaCombinaciones(Aux,Ac2,Res).

generaComb2(_I,J,Ac,Ac):-
    J>20,!.

generaComb2(I,J,Ac,Res):-
    Aux is J+1,
    generaComb2(I,Aux,[[I,J,0]|Ac],Res).

merge_sort( [], [] ):-!.             % The empty list is by definition ordered
merge_sort( [X], [X] ):-!.           % So is a list of length one
merge_sort( Unsorted, Sorted ) :- % otherwise...
    even_odd( Unsorted, L, R ), % partition the list into 2 halves
    merge_sort(L,L1),                   % recursively sort the left half
    merge_sort(R,R1),                   % recursively sort the right half
    merge1(L1,R1,Sorted).           % merge the newly-sorted halves

merge1([],L,L):-!.        % merging to empty lists is trivial
merge1(L,[] ,L ):-L\=[],!.        % merging an empty list and a non-empty list is easy
merge1([[X,Val1]|T1],[[Y,Val2]|T2],[[X,Val1]|T]):-Val1>=Val2,merge1(T1,[[Y,Val2]|T2],T),!.
merge1([[X,Val1]|T1],[[Y,Val2]|T2],[[Y,Val2]|T]):-Val1<Val2,merge1([[X,Val1]|T1],T2,T).

even_odd([],[],[]):-!.
even_odd([H|T],E,[H|O]):-even_odd(T,O,E).

individuoPartidos(Cont,Res,Res):-
    Cont=:=10,!. %Partidos por jornada

individuoPartidos(Cont,Ac,Res):-
    random_between(1,20,X),
    random_between(1,20,Y),
    random_between(0,1,Z),
    visitante(Z,Aux),
    Aux2 is Cont +1,
    individuoPartidos(Aux2,[[X,Y,Aux]|Ac],Res).

individuoJornadas(Cont,Res,Res):-
    Cont=:=38,!.   %Total de jornadas

individuoJornadas(Cont,Ac,Res):-
    Cont < 38,     %Jornadas
    individuoPartidos(0,[],Part),
    Aux is Cont +1,
    individuoJornadas(Aux,[Part|Ac],Res).

initialPopulation(Tamano,Cont,Lista,Lista):-
    Tamano =:= Cont,!.

initialPopulation(Tamano,Cont,ListasAc,Res):-
    Cont<Tamano,
    individuoJornadas(0,[],ListaP),
    Aux is Cont +1,
    initialPopulation(Tamano,Aux,[[ListaP,0]|ListasAc],Res).


scoresAux:-
    pobInicial(Lista),
    calculaTodos(Lista,[],ListaR),
    retractall(pobInicial(_)),
    asserta(pobInicial(ListaR)).

calculaTodos([],Res,Res):-!.

calculaTodos([A|B],Lista,Res):-
    scores(A,A1),                       %Checa que todos los equipos juegen 1 vez cada jornada y que no jueguen contra sí mismos
    jugar2partidos(A1,A2),              %Checa que juegue contra todos los equipos x veces
    calculaTodos(B,[A2|Lista],Res).

jugar2partidos([A,Val1],[A,Aux]):-
    combinaciones(Lista),
    cuentaPartidosJor(A,Lista,ListaVal),
    val2partidos(ListaVal,0,Val),
    Aux is Val1+Val. %Lista de todas las combinaciones, va por pares

cuentaPartidosJor([],ListaVal,ListaVal):-!.

cuentaPartidosJor([A|B],Lista,ListaVal):-
    cuentaPartidos(A,Lista,ListaVal2),
    cuentaPartidosJor(B,ListaVal2,ListaVal).

cuentaPartidos([],Lista,Lista):-!.

cuentaPartidos([[X,Y,_]|B],Lista,ListaVal):-
    X=:=Y,
    cuentaPartidos(B,Lista,ListaVal),!.

cuentaPartidos([[X,Y,_]|B],Lista,ListaVal):-
    X=\=Y,
    min(X,Y,X1,Y1),
    suma(X1,Y1,Lista,ListaVal2),
    cuentaPartidos(B,ListaVal2,ListaVal),!.

suma(X1,Y1,[[X,Y,Cont]|B],[[X,Y,Cont2]|B]):-
    X1=:=X,
    Y1=:=Y,
    Cont2 is Cont +1,!.

suma(X1,Y1,[[X,Y,Cont]|B],[[X,Y,Cont]|Res]):-
    suma(X1,Y1,B,Res).

suma2(X1,[[X,Cont]|B],[[X,Cont2]|B]):-
    X1=:=X,
    Cont2 is Cont +1,!.

suma2(X1,[[X,Cont]|B],[[X,Cont]|Res]):-
    suma2(X1,B,Res).


val2partidos([],Val,Val):-!.                                %numero de veces que debe jugar contra cada equipo

val2partidos([[_,_,Cont]|B],Ac,Val):-
    nPartidos(Cont,Val1),
    Ac2 is Ac+Val1,
    val2partidos(B,Ac2,Val).

nPartidos(2,150):-!.

nPartidos(Cont,Val):-
    Val is abs(2-Cont)*(-5).

scores([A,_],[A,Val]):-
    numPartidos(ListaP),
    dosJuegosEquipo(A,ListaP,ListaP2,0,Val1),
    juegos38(ListaP2,0,Val2),
    Val is Val1+Val2,!.
/*
dosJuegos(_,Cont,Val,Val):-
    Cont>20,!.  %Total de equipos


dosJuegos(A,Cont,Aux,Val):- %Hacer que en una iteracion cheque todos
    Cont=<20,   %Equipos
    dosJuegosEquipo(A,Cont,0,TotAc,0,Val1),
    juegos38(TotAc,Val2),
    Cont2 is Cont +1,
    Aux2 is Aux + Val1+Val2,
    dosJuegos(A,Cont2,Aux2,Val).
*/

juegos38([],Val,Val):-
    !.    %Partidos totales por equipo en el torneo

juegos38([[_,Cont]|B],Ac,Val):-
    Cont=:=38,
    Ac2 is Ac+100,
    juegos38(B,Ac2,Val),!.

juegos38([[_,Cont]|B],Ac,Val):-
    Aux is abs(38-Cont),  %Total de partidos
    Ac2 is Ac+(-30)*Aux,
    juegos38(B,Ac2,Val).

dosJuegosEquipo([],ListaP,ListaP,Val,Val):-!.

dosJuegosEquipo([A|B],ListaP,ListaP2,Aux,Val):-
    numPartidos(ListaJ),
    dosJuegosJor(A,ListaP,ListaP3,ListaJ,ListaJ2,0,Val1),
    unJuegoPorJornada(ListaJ2,0,Val2),
    Aux2 is Aux + Val1+Val2,
    dosJuegosEquipo(B,ListaP3,ListaP2,Aux2,Val).

dosJuegosJor([],ListaP,ListaP,ListaJ,ListaJ,Val,Val):-!.

dosJuegosJor([[X,Y,_]|B],ListaP,ListaP2,ListaJ,ListaJ2,Aux,Val):-
    X=:=Y,
    Aux2 is Aux -150,
    dosJuegosJor(B,ListaP,ListaP2,ListaJ,ListaJ2,Aux2,Val),!.

dosJuegosJor([[X,Y,_]|B],ListaP,ListaP2,ListaJ,ListaJ2,Aux,Val):-
    X=\=Y,
    suma2(X,ListaP,ListaP3),
    suma2(X,ListaJ,ListaJ3),
    suma2(Y,ListaP3,ListaP4),
    suma2(Y,ListaJ3,ListaJ4),
    dosJuegosJor(B,ListaP4,ListaP2,ListaJ4,ListaJ2,Aux,Val),!.

unJuegoPorJornada([],Val,Val):-
    !.  %Un juego por jornada

unJuegoPorJornada([[_,Cont]|B],Ac,Val):-
    Cont=:=1,
    Ac2 is Ac + 100,
    unJuegoPorJornada(B,Ac2,Val),!.

unJuegoPorJornada([[_,Cont]|B],Ac,Val):-
    Aux is abs(1-Cont),
    Ac2 is Ac + Aux*(-20),
    unJuegoPorJornada(B,Ac2,Val).

crossover(_Lista,Cont,Lim,ListaRes,ListaRes):-
    Cont=:=Lim,!.

crossover(Lista,Cont,Lim,ListaAux,ListaRes):-
    Cont<Lim,
    Tot is Lim*2,
    random_between(1,Tot,X),
    random_between(1,Tot,Y),
    recuperaSol(Lista,1,X,[Sol1,_]),
    recuperaSol(Lista,1,Y,[Sol2,_]),
    even_odd(Sol1,Mitad1Sol1,Mitad2Sol1),  %mitades si el numero de jornadas es par
    even_odd(Sol2,Mitad1Sol2,Mitad2Sol2),  %Si no cambiar por even_odd(Sol1,Mitad1Sol1,Mitad2Sol1),
    append(Mitad1Sol1,Mitad2Sol2,Hijo1),
    append(Mitad1Sol2,Mitad2Sol1,Hijo2),
    Aux is Cont +1,
    crossover(Lista,Aux,Lim,[[Hijo1,0],[Hijo2,0]|ListaAux],ListaRes).

mutation([],ListaRes,ListaRes):-!.

mutation([[A|_]|B],ListaAux,ListaRes):-
    random_between(1,38,X),    %38 nùmero de jornadas
    random_between(1,38,Y),
    min(X,Y,X1,Y1),
    intercambiaJornadas(A,X1,Y1,1,[],_Jor1,_Jor2,ListaAux2),
    mutation(B,[[ListaAux2,0]|ListaAux],ListaRes).

min(X,Y,X,Y):-
    X=<Y,!.

min(X,Y,Y,X).

intercambiaJornadas([],_X,_Y,_Cont,ListaRes,_,_,ListaRes):-!.

intercambiaJornadas([A|B],X,Y,Cont,ListaAux2,A3,Jor2,ListaRes):-
    X=:=Cont,
    X=\=Y,
    Aux is Cont+1,
    random_between(1,10,Rand),           %Total de partidos por jornada
    mutaJornadas(A,Rand,[],A1),
    random_between(1,100,X4),
    random_between(1,100,X5),
    switchRival(X4,A1,A2),
    switchRival(X5,A2,A3),
    intercambiaJornadas(B,X,Y,Aux,[Jor2|ListaAux2],A3,Jor2,ListaRes),!.

intercambiaJornadas([A|B],X,Y,Cont,ListaAux2,Jor1,A3,ListaRes):-
    Y=:=Cont,
    X=\=Y,
    Aux is Cont+1,
    random_between(1,10,Rand),           %Partidos por jornada
    mutaJornadas(A,Rand,[],A1),
    random_between(1,100,X4),
    random_between(1,100,X5),
    switchRival(X4,A1,A2),
    switchRival(X5,A2,A3),
    intercambiaJornadas(B,X,Y,Aux,[Jor1|ListaAux2],Jor1,A3,ListaRes),!.

intercambiaJornadas([A|B],X,Y,Cont,ListaAux2,Jor1,Jor2,ListaRes):-
    Aux is Cont+1,
    random_between(1,10,Rand),           %%
    mutaJornadas(A,Rand,[],A1),
    random_between(1,100,X4),
    random_between(1,100,X5),
    random_between(1,100,X6),
    switchRival(X4,A1,A2),
    switchRival(X5,A2,A3),
    switchRival(X6,A3,A4),
    intercambiaJornadas(B,X,Y,Aux,[A4|ListaAux2],Jor1,Jor2,ListaRes).

switchRival(X,A,A1):-
    X=<40,
    random_between(1,10,X1),
    random_between(1,10,X2),
    X1=\=X2,
    min(X1,X2,X3,X4),
    switch(1,X3,X4,[],_Eq1,_Eq2,A,A1),!.

switchRival(_X,A,A).

switch(_Cont,_X1,_X2,ListaRes,_,_,[],ListaRes):-!.

switch(Cont,X1,X2,ListaAux,B,Eq2,[[A,B,C]|Z],ListaRes):-
    Cont=:=X1,
    Aux is Cont+1,
    random_between(1,10,K),
    switchMatch(K,A,Eq2,M1,M2),
    switch(Aux,X1,X2,[[M1,M2,C]|ListaAux],B,Eq2,Z,ListaRes),!.

switch(Cont,X1,X2,ListaAux,Eq1,B,[[A,B,C]|Z],ListaRes):-
    Cont=:=X2,
    Aux is Cont+1,
    random_between(1,10,K),
    switchMatch(K,A,Eq1,M1,M2),
    switch(Aux,X1,X2,[[M1,M2,C]|ListaAux],Eq1,B,Z,ListaRes),!.

switch(Cont,X1,X2,ListaAux,Eq1,Eq2,[[A,B,C]|Z],ListaRes):-
    Aux is Cont+1,
    random_between(1,10,K),
    switchMatch(K,A,B,M1,M2),
    switch(Aux,X1,X2,[[M1,M2,C]|ListaAux],Eq1,Eq2,Z,ListaRes).

switchMatch(K,A,B,A,B):-
    K=<7,!.

switchMatch(_,A,B,B,A).

mutaJornadas(JorA,Rand,_,JorA):-
    Rand=<7,!.

mutaJornadas([],_Rand,JorAc,JorAc):-!.

mutaJornadas([[A,B,C]|Z],Rand,JorAc,JorAF):-
    random_between(1,1000,X1),
    random_between(1,1000,X2),
    random_between(1,1000,X3),
    cambiaEquipo(A,X1,A1),
    cambiaEquipo(B,X2,B1),
    cambiaVisitante(C,X3,C1),
    mutaJornadas(Z,Rand,[[A1,B1,C1]|JorAc],JorAF).

cambiaEquipo(_A,X1,A1):-
    X1=<10,
    random_between(1,20,A1),!.         %Numero de equipos

cambiaEquipo(A,_X1,A).

cambiaVisitante(A,X1,A):-
    X1>20,!.

cambiaVisitante(A,_X1,A1):-
    visitante(Num,A),
    Aux is abs(1-Num),
    visitante(Aux,A1).

recuperaSol([A|_],Cont,Lim,A):-
    Cont=:=Lim,!.

recuperaSol([_|B],Cont,Lim,Res):-
    Cont<Lim,
    Aux is Cont +1,
    recuperaSol(B,Aux,Lim,Res).

mitades(Lista,Mitad1,Mitad2):-
    append(Mitad1,Mitad2,Lista),
    length(Mitad1,N),
    length(Mitad2,N),!.

selection(Lista,Lista2):-
    mitades(Lista,Lista2,_).

geneticAlgorithm(_,Cont,Lim,_,_,[[A,Val]|B],A,Val):-
    Cont=:=Lim,
    write(Cont),
    retractall(pobFinal(_)),
    asserta(pobFinal([[A,Val]|B])),
    %calculaTodos(Lista,[],ListaVal),
    %merge_sort(ListaVal,[[A|Val]|_]),
    !.

geneticAlgorithm(Objetivo,Cont,Lim,_,_,[[A,Val]|B],A,Val):-
    Cont<Lim,
    %calculaTodos(Lista,[],ListaVal),
    %ordenaLista(ListaVal,[[A,Val]|_]),
    Val > Objetivo,write(Cont),
    retractall(pobFinal(_)),
    asserta(pobFinal([[A,Val]|B])),
    !.

geneticAlgorithm(Objetivo,Cont,Lim,Mut,Cross,Lista,Sol,ValFinal):-
    Cont<Lim,
    crossover(Lista,0,Cross,[],ListaCross),
    mutation(ListaCross,[],ListaM),
    %mutation(ListaM,[],ListaM2),
    calculaTodos(ListaM,[],ListaValM),
    append(Lista,ListaValM,ListaT),
    merge_sort(ListaT,ListaValOrd),
    selection(ListaValOrd,ListaRed),
    Cont2 is Cont +1,
    geneticAlgorithm(Objetivo,Cont2,Lim,Mut,Cross,ListaRed,Sol,ValFinal).

startGeneticAlgorithm(Objetivo,Lim,Sol,ValFinal):-
    pobInicial(Lista),
    length(Lista,X),
    Aux is X/2,
    geneticAlgorithm(Objetivo,0,Lim,Aux,Aux,Lista,Sol,ValFinal).
    %write(Sol).

auxCrossover(Lim,ListaCross):-
    pobInicial(Lista),
    crossover(Lista,0,Lim,[],ListaCross),
    retractall(pobFinal(_)),
    asserta(pobFinal(ListaCross)).

saveSolution:-
    retractall(pobInicial(_)),
    pobFinal(Lista),
    asserta(pobInicial(Lista)).

initialPopulation(Tamano,Lista):-
    generaCombinaciones(1,[],Res),
    retractall(combinaciones(_)),
    asserta(combinaciones(Res)),
    initialPopulation(Tamano,0,[],Lista),
    calculaTodos(Lista,[],ListaVal),
    merge_sort(ListaVal,ListaR),
    retractall(pobInicial(_)),
    asserta(pobInicial(ListaR)).
    %asserta(pobInicial(ListaVal)).

imprime(Num,0):-
    pobInicial(L),
    imprimeIndividuo(Num,1,L).

imprime(Num,1):-
    pobFinal(L),
    imprimeIndividuo(Num,1,L).

imprimeVal:-
    pobFinal(L),
    imprimeVal(L).

imprimeVal([]):-!.

imprimeVal([[_,Val]|B]):-
    write(Val),
    write("\n"),
    imprimeVal(B).

imprimeIndividuo(Num,Cont,[_|Cola]):-
    Cont<Num,
    Aux is Cont +1,
    imprimeIndividuo(Num,Aux,Cola).

imprimeIndividuo(Num,Cont,[[A,Val]|_]):-
    Cont=:=Num,
    write("Valor: "),
    write(Val),
    imprimeJor(1,A).

imprimeJor(Cont,[A|B]):-
    Cont=<38,             %Numero de jornadas
    write("Jornada: "),
    write(Cont),
    write("\n"),
    imprimePartidos(1,A),
    write("\n"),
    Aux is Cont +1,
    imprimeJor(Aux,B).

imprimeJor(_,_):-!.

imprimePartidos(Cont,[[X,Y,Z]|Cola]):-
    Cont=<10,             %Numero de partidos por jornada
    %equipos(X,A),        %Imprime el nombre del equipo
    write(X),
    write(" vs "),
    %equipos(Y,B),
    write(Y),
    write(" "),
    write(Z),
    write("     "),
    Aux is Cont +1,
    imprimePartidos(Aux,Cola).

imprimePartidos(_,_):-!.











