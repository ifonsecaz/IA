:-dynamic pobInicial/1.
:-dynamic pobFinal/1.
:-dynamic combinaciones/1.


equipos(1,a). %el segundo valor va el nombre del equipo
equipos(2,b).
equipos(3,c).
equipos(4,d).
equipos(5,e).
equipos(6,f).
equipos(7,g).
equipos(8,h).
equipos(9,i).
equipos(10,j).
equipos(11,k).
equipos(12,l).
equipos(13,m).
equipos(14,n).
equipos(15,o).
equipos(16,p).
equipos(17,q).
equipos(18,r).

%Lista para contar el número de partidos de cada equipo
numPartidos([[1,0],[2,0],[3,0],[4,0],[5,0],[6,0],[7,0],[8,0],[9,0],[10,0],[11,0],[12,0],[13,0],[14,0],[15,0],[16,0],[17,0],[18,0]]).

visitante(1,visitante).
visitante(0,local).

%Lista para contar el número de veces que juegan entre sí dos equipos
generaCombinaciones(Cont,Ac,Ac):-
    Cont>18,!.

generaCombinaciones(Cont,Ac,Res):-
    Cont=<18,
    Aux is Cont+1,
    generaComb2(Cont,Aux,Ac,Ac2),
    generaCombinaciones(Aux,Ac2,Res).

generaComb2(_I,J,Ac,Ac):-
    J>18,!.

generaComb2(I,J,Ac,Res):-
    Aux is J+1,
    generaComb2(I,Aux,[[I,J,0]|Ac],Res).

%Orden las soluciones
merge_sort( [], [] ):-!.
merge_sort( [X], [X] ):-!.
merge_sort( Unsorted, Sorted ) :-
    even_odd( Unsorted, L, R ),
    merge_sort(L,L1),
    merge_sort(R,R1),
    merge1(L1,R1,Sorted).

merge1([],L,L):-!.
merge1(L,[] ,L ):-L\=[],!.
merge1([[X,Val1]|T1],[[Y,Val2]|T2],[[X,Val1]|T]):-Val1>=Val2,merge1(T1,[[Y,Val2]|T2],T),!.
merge1([[X,Val1]|T1],[[Y,Val2]|T2],[[Y,Val2]|T]):-Val1<Val2,merge1([[X,Val1]|T1],T2,T).

%Divide la lista de soluciones en 2
even_odd([],[],[]):-!.
even_odd([H|T],E,[H|O]):-even_odd(T,O,E).

%%%%Generar población inicial
%
% Para generar cada partido elige dos números aleatorios que deciden los
% equipos de 1 a 18
% Un tercero para decidir si el primer equipo es visitante o local
individuoPartidos(Cont,Res,Res):-
    Cont=:=9,!. %Partidos por jornada

individuoPartidos(Cont,Ac,Res):-
    random_between(1,18,X),
    random_between(1,18,Y),
    random_between(0,1,Z),
    visitante(Z,Aux),
    Aux2 is Cont +1,
    individuoPartidos(Aux2,[[X,Y,Aux]|Ac],Res).


%Recorre las 17 jornadas
individuoJornadas(Cont,Res,Res):-
    Cont=:=17,!.   %Total de jornadas

individuoJornadas(Cont,Ac,Res):-
    Cont < 17,     %Jornadas
    individuoPartidos(0,[],Part),
    Aux is Cont +1,
    individuoJornadas(Aux,[Part|Ac],Res).

%Llama a construir x soluciones
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

%%%%Calcula el valor de la solución
%
%   Recorre de una en una las soluciones
calculaTodos([],Res,Res):-!.

calculaTodos([A|B],Lista,Res):-
    scores(A,A1),                       %Checa que todos los equipos juegen 1 vez cada jornada y que no jueguen contra sí mismos
    jugar2partidos(A1,A2),              %Checa que juegue contra todos los equipos x veces
    calculaTodos(B,[A2|Lista],Res).

%Saca la lista de todos los partidos posibles y la manda para contar
jugar2partidos([A,Val1],[A,Aux]):-
    combinaciones(Lista),
    cuentaPartidosJor(A,Lista,ListaVal),
    val2partidos(ListaVal,0,Val),
    Aux is Val1+Val.

%Recorre todas las jornadas
cuentaPartidosJor([],ListaVal,ListaVal):-!.

cuentaPartidosJor([A|B],Lista,ListaVal):-
    cuentaPartidos(A,Lista,ListaVal2),
    cuentaPartidosJor(B,ListaVal2,ListaVal).

%Cada partido, siempre que no sea contra uno mismo, suma en uno la lista
cuentaPartidos([],Lista,Lista):-!.

cuentaPartidos([[X,Y,_]|B],Lista,ListaVal):-
    X=:=Y,
    cuentaPartidos(B,Lista,ListaVal),!.

cuentaPartidos([[X,Y,_]|B],Lista,ListaVal):-
    X=\=Y,
    min(X,Y,X1,Y1), %La lista está ordenada, el menor va primero, ej. 1 vs 2
    suma(X1,Y1,Lista,ListaVal2),
    cuentaPartidos(B,ListaVal2,ListaVal),!.

%Suma un partido, Ej a 1 vs 2, solo debe tener máximo un partido
suma(X1,Y1,[[X,Y,Cont]|B],[[X,Y,Cont2]|B]):-
    X1=:=X,
    Y1=:=Y,
    Cont2 is Cont +1,!.

suma(X1,Y1,[[X,Y,Cont]|B],[[X,Y,Cont]|Res]):-
    suma(X1,Y1,B,Res).

%Suma un partido por equipo, ej 1 vs 2, le suma 1 a el equipo 1 y al 2
suma2(X1,[[X,Cont]|B],[[X,Cont2]|B]):-
    X1=:=X,
    Cont2 is Cont +1,!.

suma2(X1,[[X,Cont]|B],[[X,Cont]|Res]):-
    suma2(X1,B,Res).

%Calcula el valor, según cuantos juegos de cada combinación hubo
val2partidos([],Val,Val):-!.                                %numero de veces que debe jugar contra cada equipo

val2partidos([[_,_,Cont]|B],Ac,Val):-
    nPartidos(Cont,Val1),
    Ac2 is Ac+Val1,
    val2partidos(B,Ac2,Val).

%Si un rival se enfrentó solo una vez contra otro le suma
nPartidos(1,150):-!.

%Si se enfrentó más de una vez le resta
nPartidos(Cont,Val):-
    Val is abs(1-Cont)*(-5).

% Recorre la lista y cuenta cuantos partidos tuvieron en total y por
% jornada cada equipo
scores([A,_],[A,Val]):-
    numPartidos(ListaP),
    dosJuegosEquipo(A,ListaP,ListaP2,0,Val1),
    juegos38(ListaP2,0,Val2),
    Val is Val1+Val2,!.

%Checa si tuvieron 17 juegos en total cada equipo, si sí le suma
juegos38([],Val,Val):-
    !.

juegos38([[_,Cont]|B],Ac,Val):-
    Cont=:=17,
    Ac2 is Ac+100,
    juegos38(B,Ac2,Val),!.

%Tuvieron más o menos juegos les resta
juegos38([[_,Cont]|B],Ac,Val):-
    Aux is abs(17-Cont),  %Total de partidos
    Ac2 is Ac+(-30)*Aux,
    juegos38(B,Ac2,Val).

%Recorre las jornadas
dosJuegosEquipo([],ListaP,ListaP,Val,Val):-!.

% Saca de numPartidos una lista para contar los partidos de cada equipo
% por jornada
%Al final llama a unJuegoPorJornada
dosJuegosEquipo([A|B],ListaP,ListaP2,Aux,Val):-
    numPartidos(ListaJ),
    dosJuegosJor(A,ListaP,ListaP3,ListaJ,ListaJ2,0,Val1),
    unJuegoPorJornada(ListaJ2,0,Val2),
    Aux2 is Aux + Val1+Val2,
    dosJuegosEquipo(B,ListaP3,ListaP2,Aux2,Val).

%Recorre todos los partidos de una jornada
dosJuegosJor([],ListaP,ListaP,ListaJ,ListaJ,Val,Val):-!.

%Si juega contra sí mismo le resta
dosJuegosJor([[X,Y,_]|B],ListaP,ListaP2,ListaJ,ListaJ2,Aux,Val):-
    X=:=Y,
    Aux2 is Aux -150,
    dosJuegosJor(B,ListaP,ListaP2,ListaJ,ListaJ2,Aux2,Val),!.

%Suma los juegos a las listas
dosJuegosJor([[X,Y,_]|B],ListaP,ListaP2,ListaJ,ListaJ2,Aux,Val):-
    X=\=Y,
    suma2(X,ListaP,ListaP3),
    suma2(X,ListaJ,ListaJ3),
    suma2(Y,ListaP3,ListaP4),
    suma2(Y,ListaJ3,ListaJ4),
    dosJuegosJor(B,ListaP4,ListaP2,ListaJ4,ListaJ2,Aux,Val),!.

%Checa que jugó una vez en la jornada cada equipo
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

%%%Cruzamiento
% Toma la mitad de las jornadas de uno y las intercambia con otro
% elegido aleatoriamente
crossover(_Lista,Cont,Lim,ListaRes,ListaRes):-
    Cont=:=Lim,!.

crossover(Lista,Cont,Lim,ListaAux,ListaRes):-
    Cont<Lim,
    Tot is Lim*2,
    random_between(1,Tot,X), %Elige una solución aleatoriamente
    random_between(1,Tot,Y),
    recuperaSol(Lista,1,X,[Sol1,_]),
    recuperaSol(Lista,1,Y,[Sol2,_]),
    even_odd(Sol1,Mitad1Sol1,Mitad2Sol1),  %mitades si el numero de jornadas es par
    even_odd(Sol2,Mitad1Sol2,Mitad2Sol2),  %Si no cambiar por even_odd(Sol1,Mitad1Sol1,Mitad2Sol1),
    append(Mitad1Sol1,Mitad2Sol2,Hijo1),
    append(Mitad1Sol2,Mitad2Sol1,Hijo2),
    Aux is Cont +1,
    crossover(Lista,Aux,Lim,[[Hijo1,0],[Hijo2,0]|ListaAux],ListaRes).

%%%Mutación
%Para una solución puede intercambiar dos jornadas de lugar
% Puede cambiar dos rivales (1 vs 2 y 4 vs 3 -> 1 vs 3 y 4 vs 2), el
% orden del partido (1 vs 2 -> 2 vs 1), y cambiar alguno de los dos
% equipos y si es visitante por otro aleatoriamente
mutation([],ListaRes,ListaRes):-!.

mutation([[A|_]|B],ListaAux,ListaRes):-
    random_between(1,17,X),    %nùmero de jornadas
    random_between(1,17,Y),
    min(X,Y,X1,Y1),
    intercambiaJornadas(A,X1,Y1,1,[],_Jor1,_Jor2,ListaAux2),
    mutation(B,[[ListaAux2,0]|ListaAux],ListaRes).

%
min(X,Y,X,Y):-
    X=<Y,!.

min(X,Y,Y,X).

%Cambia de posición dos jornadas
intercambiaJornadas([],_X,_Y,_Cont,ListaRes,_,_,ListaRes):-!.

intercambiaJornadas([A|B],X,Y,Cont,ListaAux2,A3,Jor2,ListaRes):-
    X=:=Cont,
    X=\=Y,
    Aux is Cont+1,
    random_between(1,9,Rand),           %Total de partidos por jornada
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
    random_between(1,9,Rand),           %Partidos por jornada
    mutaJornadas(A,Rand,[],A1),
    random_between(1,100,X4),
    random_between(1,100,X5),
    %random
    switchRival(X4,A1,A2),
    switchRival(X5,A2,A3),
    intercambiaJornadas(B,X,Y,Aux,[Jor1|ListaAux2],Jor1,A3,ListaRes),!.

intercambiaJornadas([A|B],X,Y,Cont,ListaAux2,Jor1,Jor2,ListaRes):-
    Aux is Cont+1,
    random_between(1,9,Rand),           %%
    mutaJornadas(A,Rand,[],A1),
    random_between(1,100,X4),
    random_between(1,100,X5),
    random_between(1,100,X6),
    switchRival(X4,A1,A2),
    switchRival(X5,A2,A3),
    switchRival(X6,A3,A4),
    intercambiaJornadas(B,X,Y,Aux,[A4|ListaAux2],Jor1,Jor2,ListaRes).

%Intercambia dos rivales aleatoriamente
switchRival(X,A,A1):-
    X=<40,
    random_between(1,9,X1),
    random_between(1,9,X2),
    X1=\=X2,
    min(X1,X2,X3,X4),
    switch(1,X3,X4,[],_Eq1,_Eq2,A,A1),!.

switchRival(_X,A,A).

%Cambia de posición los rivales que se enfrentan
%2 vs 5 -> 5 vs 2 (Ayuda a switchRival)
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
    K=<5,!.

switchMatch(_,A,B,B,A).

%Para cambiar valores dentro de una jornada (rivales)
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

%Cambia aleatoriamente uno de los enfrentamientos
cambiaEquipo(_A,X1,A1):-
    X1=<15,
    random_between(1,18,A1),!.         %Numero de equipos

cambiaEquipo(A,_X1,A).

%Cambia si es visitante o local el primero de los equipos
cambiaVisitante(A,X1,A):-
    X1>20,!.

cambiaVisitante(A,_X1,A1):-
    visitante(Num,A),
    Aux is abs(1-Num),
    visitante(Aux,A1).

%Crossover
%Saca una de las soluciones de la lista
recuperaSol([A|_],Cont,Lim,A):-
    Cont=:=Lim,!.

recuperaSol([_|B],Cont,Lim,Res):-
    Cont<Lim,
    Aux is Cont +1,
    recuperaSol(B,Aux,Lim,Res).

%Toma la mitad de una lista
mitades(Lista,Mitad1,Mitad2):-
    append(Mitad1,Mitad2,Lista),
    length(Mitad1,N),
    length(Mitad2,N),!.

%Toma la mitad de las soluciones
% Como ya están ordenadas, toma las x mejores según el tamaño de la
% población inicial
selection(Lista,Lista2):-
    mitades(Lista,Lista2,_).

%Llama al curzamiento, mutación, calcula valores
%Si llegó al límite de generaciones
geneticAlgorithm(_,Cont,Lim,_,_,[[A,Val]|B],A,Val):-
    Cont=:=Lim,
    write(Cont),
    retractall(pobFinal(_)),
    asserta(pobFinal([[A,Val]|B])),
    %calculaTodos(Lista,[],ListaVal),
    %merge_sort(ListaVal,[[A|Val]|_]),
    !.

%Si llegó al objetivo
geneticAlgorithm(Objetivo,Cont,Lim,_,_,[[A,Val]|B],A,Val):-
    Cont<Lim,
    %calculaTodos(Lista,[],ListaVal),
    %ordenaLista(ListaVal,[[A,Val]|_]),
    Val > Objetivo,write(Cont),
    retractall(pobFinal(_)),
    asserta(pobFinal([[A,Val]|B])),
    !.

% Primero hace el cruzamiento y la mutación, estos generan una población
% de tamaño x igual a la inicial, por ejemplo 100 y se calcula su valor
% después se junta con la generación anterior, se ordenan y se toman las
% x mejores
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

%Llama a iniciar el algoritmo genético
startGeneticAlgorithm(Objetivo,Lim,Sol,ValFinal):-
    pobInicial(Lista),
    length(Lista,X),
    Aux is X/2,
    geneticAlgorithm(Objetivo,0,Lim,Aux,Aux,Lista,Sol,ValFinal).
    %write(Sol).

% La población final ahora se convierte en la inicial, permite volver a
% llamar a geneticAlgorithm desde donde se quedó, en lugar de iniciar
% desde 0
saveSolution:-
    retractall(pobInicial(_)),
    pobFinal(Lista),
    asserta(pobInicial(Lista)).

%Llama a crear la población inicial
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

%Imprime una solución de la población inicial o final
imprime(Num,0):-
    pobInicial(L),
    imprimeIndividuo(Num,1,L).

imprime(Num,1):-
    pobFinal(L),
    imprimeIndividuo(Num,1,L).

%Imprime los valores de la población final
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
    Cont=<17,             %Numero de jornadas
    write("Jornada: "),
    write(Cont),
    write("\n"),
    imprimePartidos(1,A),
    write("\n"),
    Aux is Cont +1,
    imprimeJor(Aux,B).

imprimeJor(_,_):-!.

imprimePartidos(Cont,[[X,Y,Z]|Cola]):-
    Cont=<9,             %Numero de partidos por jornada
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

















