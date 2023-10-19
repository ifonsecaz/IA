:- dynamic fichasJugador/3. %Id 1 es la computadora, 2 el otro jugador
:- dynamic fichasJugadas/5. %Representa la mesa de juego

%Mark(i,i,i,o,o,o).
%Saca las posibles tiradas
%ListaP lista del jugador
%Tirada 1 y 2 movimientos permitidos
%X, Y fichas válidas
%Posicion dónde se coloca
mark(ListaP,Tirada1,Tirada2,X,Y,PosicionFicha) :-
    busca(ListaP,Tirada1,Tirada2,X,Y,PosicionFicha).

% Método busca recorre la lista de los jugadores buscando las tiradas
% válidas
% busca(i,i,i,o,o,o)
busca([X,Y|_],X,_,X,Y,X).

busca([X,Y|_],Y,_,X,Y,Y).

busca([X,Y|_],Aux,X,X,Y,X):-
    Aux =\=X.
  %Evita repetir tiradas
busca([X,Y|_],Aux,Y,X,Y,Y):-
    Aux =\=Y.
busca([_,_|Z],X,Y,X1,Y1,PosFicha):-
    busca(Z,X,Y,X1,Y1,PosFicha).

%Es llamada cuando el jugador2 hace una tirada o come
% Guarda todas las fichas que no están en el tablero o que tiene el
% jugador 1,
% posiblesFichas(2)
% Primero agrega una lista con todas las opciones
posiblesFichas(NumJugador,Cant):-
    Lista = [6,6,6,5,6,4,6,3,6,2,6,1,6,0,5,5,5,4,5,3,5,2,5,1,5,0,4,4,4,3,4,2,4,1,4,0,3,3,3,2,3,1,3,0,2,2,2,1,2,0,1,1,1,0,0,0],
    fichasJugador(FichasJugador1,Cont,1),
    fichasJugadas(FichasJugadas,Cont2,_,_,1),
    descartaPosiblesFichas(FichasJugadas,Lista,0,Cont2,ListaR),
    descartaPosiblesFichas(FichasJugador1,ListaR,0,Cont,ListaR2),
    %Aux is 28- Cont - Cont2,
    retract(fichasJugador(_,_,NumJugador)),
    asserta(fichasJugador(ListaR2,Cant,NumJugador)).

%descartaPosibleFichas(i,i,i,i,o)
%Quita de la lista del jugador2 las fichas dadas contenidas en Lista2
descartaPosiblesFichas([],Res,Lim,Lim,Res):-!.

descartaPosiblesFichas([X,Y|Z],Lista2,Cont,Lim,Res):-
    Cont<Lim,
    elimina(X,Y,Lista2,NLista),
    Aux is Cont+1,
    descartaPosiblesFichas(Z,NLista,Aux,Lim,Res).

%oponenteComio(i,i)
% Solo se descartan las fichas que no tenía
oponenteComio(Ficha1,Ficha2,Aux):-
    retract(fichasJugador(Lista,_Cant,2)),
    descartaNumeroR(Lista,0,Ficha1,Total,ListaR),
    descartaNumeroR(ListaR,Total,Ficha2,_,ListaR2),
    %Aux is Cant - Total2,
    asserta(fichasJugador(ListaR2,Aux,2)).

%descartaNumeroR(i,i,i,o,o)
descartaNumeroR([],Total,_,Total,[]).

descartaNumeroR([X,_|Z],Cont,X,Total,ListaR):-
    Aux is Cont +1,
    descartaNumeroR(Z,Aux,X,Total,ListaR).

descartaNumeroR([_,X|Z],Cont,X,Total,ListaR):-
    Aux is Cont +1,
    descartaNumeroR(Z,Aux,X,Total,ListaR).

descartaNumeroR([X,Y|Z],Cont,Ficha,Total,[X,Y|ListaR]):-
    X =\= Ficha,
    Y =\= Ficha,
    descartaNumeroR(Z,Cont,Ficha,Total,ListaR).

eliminaFichasAnt(X,Y):-
    Lista = [6,6,5,5,4,4,3,3,2,2,1,1,0,0,6,5,6,4,6,3,6,2,6,1,6,0,5,4,5,3,5,2,5,1,5,0,4,3,4,2,4,1,4,0,3,2,3,1,3,0,2,1,2,0,1,0],
    retract(fichasJugador(ListaJ,Cant,2)),
    eliminaFichasAntR(X,Y,Lista,ListaJ,ListaR),
    assert(fichasJugador(ListaR,Cant,2)).

eliminaFichasAntR(X,Y,[X2,Y2|_Z],ListaR,ListaR):-
    X=:=X2,
    Y=:=Y2,!.

eliminaFichasAntR(X,Y,[X2,Y2|Z],Lista,ListaR):-
    elimina(X2,Y2,Lista,ListaAux),
    eliminaFichasAntR(X,Y,Z,ListaAux,ListaR).

%agrega(i,i,i,o)
%Agrega una ficha a la lista dada
agrega(X,Y,[],[X,Y]):-!.

agrega(X,Y,[H1,H2|T],[H1,H2|Res]):-
    agrega(X,Y,T,Res).

%elimina(i,i,i,o)
%Elimina una ficha de una lista
elimina( _,_, [], []).
elimina(X,Y, [X,Y|T], T).
elimina(X,Y, [H1,H2|T], [H1,H2|T2]) :-
    H1 =\= X,
    elimina(X,Y, T, T2).

elimina(X,Y, [X,H2|T], [X,H2|T2]) :-
    H2 =\= Y,
    elimina(X,Y, T, T2).

% sigTirada(i,i,i,i,i,o,o)
% Dada la ficha tirada, devuelve cuales son las siguientes tiradas
% válidas
sigTirada(X,Y,_,[-1,_],[-1,_],[X,1],[Y,1]):-
    eliminaFichasAnt(X,Y),
    !.

sigTirada(X,Y,FichaDondeSeColoco,[Tirada1,_],[Tirada2,Ant],[Y,1],[Tirada2,Aux]):-
    FichaDondeSeColoco =:= Tirada1,
    Aux is Ant+1,
    X =:= Tirada1,!.

sigTirada(X,Y,FichaDondeSeColoco,[Tirada1,Ant],[Tirada2,_],[Y,1],[Tirada1,Aux]):-
    FichaDondeSeColoco =:= Tirada2,
    Aux is Ant+1,
    X =:= Tirada2,!.

sigTirada(X,Y,FichaDondeSeColoco,[Tirada1,_],[Tirada2,Ant],[X,1],[Tirada2,Aux]):-
    FichaDondeSeColoco =:= Tirada1,
    Aux is Ant+1,
    Y =:= Tirada1,!.

sigTirada(X,Y,FichaDondeSeColoco,[Tirada1,Ant],[Tirada2,_],[X,1],[Tirada1,Aux]):-
    FichaDondeSeColoco =:= Tirada2,
    Aux is Ant+1,
    Y =:= Tirada2,!.

% record(i,o,i,i,i,i,o)
% Llamado cuando el jugador 2 hace una tirada, actualiza el tablero y
% saca las nuevas tiradas
record(Player,Player2,X,Y,FichaDondeSeColoco,Tablero,Tablero2) :-
   fichasJugadas(Lista,Cant,Tirada1,Tirada2,Tablero),
   agrega(X,Y,Lista,Lista2),
   sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,NTirada1,NTirada2),
   Aux is Cant + 1,
   asserta(fichasJugadas(Lista2,Aux,NTirada1,NTirada2,Tablero2)),
   fichasJugador(ListaJ,Cant2,Player),
   Aux2 is Cant2 -1,
   elimina(X,Y,ListaJ,ListaJ2),
   asserta(fichasJugador(ListaJ2,Aux2,Player2)).

% record2(i,i,i,i,i,i,i,o,o,o,o)
% Llamado dentro de minimax, recibe la lista de uno de los jugadores, la
% ficha tirada y las opciones,
% Agrega la ficha a una nueva lista para el tablero
% La elimina de la lista del jugador
% Obtiene las nuevas tiradas
record2(ListaP,X,Y,FichaDondeSeColoco,Tirada1,Tirada2,ListaT,ListaT2,ListaP2,NTirada1,NTirada2) :-
   agrega(X,Y,ListaT,ListaT2),
   sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,NTirada1,NTirada2),
   elimina(X,Y,ListaP,ListaP2).

%cuentaLista(i,i,o)
%Cuenta cuantas fichas hay en la lista
cuentaLista([],Aux,Aux):-!.

cuentaLista([_,_|Z],Aux,Cont):-
    Aux2 is Aux +1,
    cuentaLista(Z,Aux2,Cont).

%get_pos_valuea(i,i,i,o)
%calcula el valor de seguir cierta ruta
get_pos_value(Turno,_ComioOLim,ListaP,_ListaP2,ListaT,[_Tirada1,Ant1],[_Tirada2,Ant2],Val):-
    cuentaLista(ListaP,0,Cant),
    opComio(Turno,Cant,Aux),
    winPos(Cant,Aux2),
    mulas(ListaP,0,CantM),
    Aux3 is CantM*(-35),
    manoVariada(ListaP,Aux4),
    fichasSinMov(ListaT,0,ListaFichaSin),
    cuentaLista1(ListaFichaSin,0,Cant2),
    Aux5 is Cant2*10,
    fichasAhogadas(ListaFichaSin,ListaP,Res),
    Aux6 is Res*(-200),
    cuentaLista(ListaT,0,CantTiradas),
    tiraAntes(Turno,Aux2,ListaT,CantTiradas,NLista),
    tirarAntesMulasR(NLista,0,CantTiradas,0,Aux7),
    bloqueo(NLista,CantTiradas,0,Aux8),
    AntT is 5*(Ant1-1) + 5*(Ant2-1),
    Val is Aux + Aux2+Aux3+AntT+Aux4+Aux5+Aux6+Aux7+Aux8.

tiraAntes(Turno,Aux2,ListaT,CantTiradas,ListaT):-
    Aux is CantTiradas mod 2,
    Aux =:= 1,
    Turno =:=1, %O gané o comí
    Aux2 =:=100, %Último tiro fue mío
    !.

tiraAntes(Turno,Aux2,[_,_|ListaT],CantTiradas,ListaT):-
    Aux is CantTiradas mod 2,
    Aux =:= 1,
    Turno =:=1,
    Aux2=\=100, %ültimo tiro oponente
    !.

tiraAntes(Turno,_Aux2,ListaT,CantTiradas,ListaT):-
    Aux is CantTiradas mod 2,
    Aux =:= 1,
    Turno =:=2, %Oponente comio, último tiro mío
    !.


tiraAntes(Turno,Aux2,[_,_|ListaT],CantTiradas,ListaT):-
    Aux is CantTiradas mod 2,
    Aux =:= 0,
    Turno =:=1, %O gané o comí
    Aux2 =:=100, %Último tiro fue mío
    !.

tiraAntes(Turno,Aux2,ListaT,CantTiradas,ListaT):-
    Aux is CantTiradas mod 2,
    Aux =:= 0,
    Turno =:=1,
    Aux2=\=100, %ültimo tiro oponente
    !.

tiraAntes(Turno,_Aux2,[_,_|ListaT],CantTiradas,ListaT):-
    Aux is CantTiradas mod 2,
    Aux =:= 0,
    Turno =:=2, %Oponente comio, último tiro mío
    !.

tirarAntesMulasR([],_,_,Val,Val):-
    number(Val),!.

tirarAntesMulasR([],_,_,_Val,0):-!.

tirarAntesMulasR([X,Y|Z],Turno,CantTiradas,Ac,Val):-
    Aux is Turno mod 2,
    Aux =:=0,
    Aux2 is Turno +1,
    X=:=Y,
    AcumVal is Ac+15*(CantTiradas-(Aux2*4)),
    tirarAntesMulasR(Z,Aux2,CantTiradas,AcumVal,Val).

tirarAntesMulasR([_X,_Y|Z],Turno,CantTiradas,Ac,Val):-
    Aux2 is Turno +1,
    tirarAntesMulasR(Z,Aux2,CantTiradas,Ac,Val).

%Primer mov es mío

bloqueo(_Lista,CantTiradas,Val,Val):-
    CantTiradas=<2,!.

bloqueo([_,_,MovOp1,MovOp2|Lista],_CantTiradas,Ac,Val):-
    %Aux is 0,
    %Aux2 is CantTiradas-2,
    bloqueoR(Lista,MovOp1,MovOp2,Ac,TirAc),
    Val is TirAc*10.

bloqueoR([],_MovOp1,_MovOp2,TirAc,TirAc):-!.


bloqueoR([X,Y|[]],MovOp1,MovOp2,Ac,TirAc):-
    sigueTirada(X,Y,MovOp1,MovOp2,Aux),
    TirAc is Ac+Aux,!.


bloqueoR([X,Y,NMov1,NMov2|Lista],MovOp1,MovOp2,Ac,TirAc):-
    sigueTirada(X,Y,MovOp1,MovOp2,Aux),
    Ac2 is Ac+Aux,
    bloqueoR(Lista,NMov1,NMov2,Ac2,TirAc).

sigueTirada(X,_Y,X,_MovOp2,1):-!.

sigueTirada(X,_Y,_,X,1):-!.

sigueTirada(_,Y,Y,_MovOp2,1):-!.

sigueTirada(_,Y,_,Y,1):-!.

sigueTirada(_,_,_,_,0).


cuentaLista1([],Cont,Cont):-!.

cuentaLista1([_|Y],Cont,Cant):-
    Aux is Cont +1,
    cuentaLista1(Y,Aux,Cant).

opComio(1,Cant,Val):-
    Val is Cant*(-5).

opComio(2,_Cant,30).

manoVariada(ListaP,Val):-
    cuentaNum(ListaP,0,0,Cant0,_),
    cuentaNum(ListaP,1,0,Cant1,_),
    cuentaNum(ListaP,2,0,Cant2,_),
    cuentaNum(ListaP,3,0,Cant3,_),
    cuentaNum(ListaP,4,0,Cant4,_),
    cuentaNum(ListaP,5,0,Cant5,_),
    cuentaNum(ListaP,6,0,Cant6,_),
    Val is 30 -(abs(Cant0-1))*4 -(abs(Cant1-1))*4 -(abs(Cant2-1))*4 -(abs(Cant3-1))*4 -(abs(Cant4-1))*4 -(abs(Cant5-1))*4 -(abs(Cant6-1))*4.

fichasSinMov(ListaT,Cont,Lista):-
    Cont =< 6,
    cuentaNum(ListaT,Cont,0,Cant,Mula),
    Mula =:= 1,
    Cant =:= 7,
    Aux is Cont +1,
    fichasSinMov(ListaT,Aux,ListaF),
    append([Cont],ListaF,Lista),!.

fichasSinMov(ListaT,Cont,Lista):-
    Cont =< 6,
    cuentaNum(ListaT,Cont,0,Cant,Mula),
    Mula =:= 0,
    Cant =:= 6,
    Aux is Cont +1,
    fichasSinMov(ListaT,Aux,ListaF),
    append([Cont],ListaF,Lista),!.

fichasSinMov(ListaT,Cont,Y):-
    Cont =< 6,
    Aux is Cont +1,
    cuentaNum(ListaT,Cont,0,Cant,Mula),
    Cant=\=7,
    Mula=\=0,
    fichasSinMov(ListaT,Aux,Y),!.

fichasSinMov(ListaT,Cont,Y):-
    Cont =< 6,
    Aux is Cont +1,
    cuentaNum(ListaT,Cont,0,Cant,Mula),
    Cant=\=6,
    Mula=\=1,
    fichasSinMov(ListaT,Aux,Y),!.


fichasSinMov(_,Cont,[]):-
    Cont>6.

fichasAhogadas([],_,0):-!.

fichasAhogadas([X|_],ListaP,Val):-
    cuentaNum(ListaP,X,0,Cant,_Mula),
    Cant=:=1,
    Val is 1,!.

fichasAhogadas([_|Y],ListaP,Val):-
    fichasAhogadas(Y,ListaP,Val).


cuentaNum([],_,Cant,Cant,Mula):-
    not(number(Mula)),
    Mula is 0,!.

cuentaNum([],_,Cant,Cant,_Mula):-!.

cuentaNum([X,Y|Z],Num,Cont,Cant,Mula):-
    X =\= Num,
    Y =\= Num,
    cuentaNum(Z,Num,Cont,Cant,Mula),!.

cuentaNum([X,Y|Z],Num,Cont,Cant,Mula):-
    X=:=Y,
    Mula is 1,
    Aux is Cont +1,
    cuentaNum(Z,Num,Aux,Cant,Mula),!.

cuentaNum([_,_|Z],Num,Cont,Cant,Mula):-
    Aux is Cont +1,
    cuentaNum(Z,Num,Aux,Cant,Mula).

%winPos(i,o)
% Checa si puede ganar por ese camino sin comer, llega cuando no le
% quedan fichas
winPos(0,100):-!.

%Da 0 si aún tiene fichas
winPos(_,0).

%mulas(i,i,o)
%Checa cuantas mulas le quedan
mulas([],Cont,Cont):-!.

mulas([X,X|Z],Cont,CantM):-
    Aux is Cont + 1,
    mulas(Z,Aux,CantM).

mulas([X,Y|Z],Cont,CantM):-
    X =\= Y,
    mulas(Z,Cont,CantM).

%moves(i,i,i,o)
% Regresa una lista de listas con las todas las tiradas válidas y la
% posición donde se colocan
moves(ListaP,[Tirada1,_],[Tirada2,_],Tiradas):-
   findall([X,Y,PosicionFicha],mark(ListaP,Tirada1,Tirada2,X,Y,PosicionFicha),Tiradas),!.

%listaEnTurno(i,i,i,o)
%Checa de quien es el turno y devuelve la lista de ese jugador
listaEnTurno(1,Lista1,_,Lista1):-!.

listaEnTurno(2,_,Lista,Lista).

%alphabeta(i,i,i,i,i,i,i,i,o,o,i,i)
%
% Para mandar un elemento desde c hasta calcula valor, se debe agregar
% en alphabeta, boundedbest y goodenough
%Caso llega a un límite
alphabeta(Turno,ListaP,ListaP2,ListaT,Tirada1,Tirada2,_,_,_, Val,Depth,Lim) :-
   Depth > Lim,
   get_pos_value(Turno,0,ListaP,ListaP2,ListaT,Tirada1,Tirada2,Val),!.

%Caso si se queda sin movimientos
alphabeta(Turno,ListaP,ListaP2,ListaT,Tirada1,Tirada2,_,_,_, Val,_,_) :-
   listaEnTurno(Turno,ListaP,ListaP2,ListaR),
   moves(ListaR,Tirada1,Tirada2,[]),
   get_pos_value(Turno,1,ListaP,ListaP2,ListaT,Tirada1,Tirada2,Val),!.

alphabeta(Turno,ListaP,ListaP2,ListaT,Tirada1,Tirada2,Alpha, Beta, GoodPos, Val,Depth,Lim) :-
   listaEnTurno(Turno,ListaP,ListaP2,ListaR),
   Depth1 is Depth +1,
   moves(ListaR,Tirada1,Tirada2,Tiradas),
   boundedbest(Turno,Tiradas,ListaP,ListaP2,ListaT,Tirada1,Tirada2, Alpha, Beta, GoodPos, Val,Depth1,Lim),!.

%Boundedbest(i,i,i,i,i,i,i,i,i,o,o,i,i)
%Saca un movimiento y llama a alphabeta para el siguiente turno
%Jugador1
boundedbest(1,[[X,Y,PosFicha]|Moves],ListaP,ListaP2,ListaT,Tirada1,Tirada2, Alpha, Beta, GoodPos, GoodVal,Depth,Lim) :-
   record2(ListaP,X,Y,PosFicha,Tirada1,Tirada2,ListaT,ListaT2,NListaP,NTirada1,NTirada2),
   alphabeta(2,NListaP,ListaP2,ListaT2,NTirada1,NTirada2,Alpha, Beta, _, Val,Depth,Lim),
   goodenough(1,[X,Y,PosFicha],Moves, Alpha, Beta,ListaP,ListaP2,ListaT,Tirada1,Tirada2,Val, GoodPos, GoodVal,Depth,Lim).

%Jugador2
boundedbest(2,[[X,Y,PosFicha]|Moves],ListaP,ListaP2,ListaT,Tirada1,Tirada2, Alpha, Beta, GoodPos, GoodVal,Depth,Lim) :-
   record2(ListaP2,X,Y,PosFicha,Tirada1,Tirada2,ListaT,ListaT2,NListaP2,NTirada1,NTirada2),
   alphabeta(1,ListaP,NListaP2,ListaT2,NTirada1,NTirada2,Alpha, Beta, _, Val,Depth,Lim),
   goodenough(2,[X,Y,PosFicha],Moves, Alpha, Beta,ListaP,ListaP2,ListaT,Tirada1,Tirada2,Val, GoodPos, GoodVal,Depth,Lim).

%goodenough(i,i,i,i,i,i,i,i,i,i,i,o,o,i,i)
%Compara alpha y beta
goodenough(_,PosList,[],_,_,_,_,_,_,_, Val, PosList, Val,_,_) :- !.

goodenough(Turno,PosList,_, _, Beta,_,_,_,_, _, Val, PosList, Val,_,_) :-
    min_to_move(Turno), Val > Beta,!.

goodenough(Turno,PosList,_, Alpha, _,_,_,_,_,_, Val, PosList, Val,_,_) :-
   max_to_move(Turno), Val < Alpha,!.

goodenough( Turno,PosList,Moves, Alpha, Beta,ListaP,ListaP2,ListaT,Tirada1,Tirada2, Val, GoodPos, GoodVal,Depth,Lim) :-
   newbounds( Alpha, Beta, Turno, Val, NewAlpha, NewBeta),
   boundedbest(Turno,Moves,ListaP,ListaP2,ListaT,Tirada1,Tirada2, NewAlpha, NewBeta, Pos1, Val1,Depth,Lim), %Evalua las otras opciones
   betterof(Turno,PosList, Val, Pos1, Val1, GoodPos, GoodVal).

%newbpunds(i,i,i,i,o,o)
%Define nueva alpha o beta
newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
   min_to_move(Pos), Val > Alpha,!.          % Mazximizer increased lower bound

newbounds(Alpha, Beta, Pos, Val, Alpha, Val):-
   max_to_move(Pos), Val < Beta,!.            % Minimizer decreased upper bound

newbounds( Alpha, Beta, _,_,Alpha, Beta).    % otherwise bounds unchanged.

%betterof(i,i,i,i,o,o,o)
betterof(PosJugador,PosList, Val, _, Val1, PosList, Val) :-          % Pos better than Pos1
   min_to_move(PosJugador), Val > Val1, !.

betterof(PosJugador,PosList, Val, _, Val1, PosList, Val) :-          % Pos better than Pos1
   max_to_move(PosJugador), Val < Val1, !.

betterof(_,_,_,Pos1,Val1,Pos1,Val1).                % otherwise Pos 1 better

%min_to_move(i), max_to_move(i)
%Checa si debe minimizar o maximizar
%2 minimiza, 1 maximiza
min_to_move(2).

max_to_move(1).

%fichasNuevoJuegoR(i,i)
%Método recursivo que pide las 7 fichas del jugador 1
fichasNuevoJuegoR(Cont,NumJugador):-
    Cont =< 7,
    write("Dame tu siguiente ficha"),
    nl,
    read(X),
    read(Y),
    retract(fichasJugador(ListaJ,_,NumJugador)),
    agrega(X,Y,ListaJ,ListaJ2),
    asserta(fichasJugador(ListaJ2,Cont,NumJugador)),
    Aux is Cont +1,
    fichasNuevoJuegoR(Aux,NumJugador).

fichasNuevoJuegoR(_,_):-!.

%Inicia una nueva partida
fichasNuevoJuego:-
    retractall(fichasJugadas(_,_,_,_,_)),
    assert(fichasJugadas([],0,[-1,1],[-1,1],1)), %-1 indica que cualquier ficha se puede jugar
    retractall(fichasJugador(_,_,_)),
    assert(fichasJugador([],0,1)),
    assert(fichasJugador([],0,2)),
    fichasNuevoJuegoR(1,1),
    posiblesFichas(2,7).

%Método para indicar cuántas fichas comió el oponente
oponenteCome:-
    nl,
    write("¿Cuántas fichas comió?"),
    read(X),
    fichasJugadas(_,_,[Ficha1|_],[Ficha2|_],1),
    fichasJugador(_,Cant,2),
    Aux is X+Cant,
    posiblesFichas(2,Aux),
    oponenteComio(Ficha1,Ficha2,Aux).

%Método para indicar que el jugador paso
oponentePaso:-
    fichasJugadas(_,_,[Ficha1|_],[Ficha2|_],1),
    fichasJugador(_,Cant,2),
    oponenteComio(Ficha1,Ficha2,Cant),!.

%Método para agregar fichas que comio,
%se debe llamar tantas veces como fichas comio
comer:-
    nl,
    write("Dame la siguiente ficha"),
    nl,
    read(X),
    read(Y),
    retract(fichasJugador(ListaJ,Cant,1)),
    agrega(X,Y,ListaJ,ListaJ2),
    Cont is Cant +1,
    asserta(fichasJugador(ListaJ2,Cont,1)).

%Método para indicar la jugada del rival
jugada:-
    nl,
    write("¿Qué ficha tiró?"),
    nl,
    read(FichaX),
    read(FichaY),
    nl,
    write("¿En qué posición la colocó?"),
    read(Pos),
    record(2,2,FichaX,FichaY,Pos,1,1),!.

%Falta método para devolver ficha más grande
%Variar límite según numero tiradas en el primer movimiento
%Para la primera jugada del otro como posición se debe indicar -1
% Falta poner caso donde debe comenzar c, un método extra que decida
% cual es la ficha más alta, no conviene dejarlo a minimax pues son
% muchas combinaciones
%
% Para dar una ficha siempre dar el número mayor primero
%
%Método para calcular el siguiente tiro
c:-
   retract(fichasJugadas(Lista,Cant,Tirada1,Tirada2,1)),
   retract(fichasJugador(ListaJ2,Cont,1)),
   retract(fichasJugador(ListaJ3,Cont2,2)),
   Depth is 1,
   ConT is Cont +Cont2,
   limite(ConT,Lim),
   fichasViejas(Tirada1,Tirada2,ListaJ3,ListaJ3R),
   alphabeta(1,ListaJ2,ListaJ3R,Lista,Tirada1,Tirada2,-200,200,[FichaX,FichaY,Pos],Value,Depth,Lim),
   write(Value),
   tirada(ListaJ2,FichaX,FichaY,Pos,Tirada1,Tirada2,Lista,ListaT2,ListaP2,NTirada1,NTirada2),
   Aux is Cant + 1,
   Aux2 is Cont-1,
   asserta(fichasJugador(ListaP2,Aux2,1)),
   asserta(fichasJugador(ListaJ3R,Cont2,2)),
   asserta(fichasJugadas(ListaT2,Aux,NTirada1,NTirada2,1)),!.

fichasViejas([Tirada1,Ant1],[_Tirada2,_Ant2],ListaJ3,ListaJ3R):-
    Ant1>=5,
    descartaNumeroR(ListaJ3,0,Tirada1,_Total,ListaJ3R),!.

fichasViejas([_Tirada1,_Ant1],[Tirada2,Ant2],ListaJ3,ListaJ3R):-
    Ant2>=5,
    descartaNumeroR(ListaJ3,0,Tirada2,_Total,ListaJ3R),!.

fichasViejas([_Tirada1,_Ant1],[_Tirada2,_Ant2],ListaJ3,ListaJ3).

cFichaMasAltaR2([X,_Y|Z],X2,Y2,FichaX,FichaY):-
    X=\=X2,
    cFichaMasAltaR2(Z,X2,Y2,FichaX,FichaY).

cFichaMasAltaR2([_X,Y|Z],X2,Y2,FichaX,FichaY):-
    Y=\=Y2,
    cFichaMasAltaR2(Z,X2,Y2,FichaX,FichaY).


cFichaMasAltaR2([X,Y|_Z],X2,Y2,X,Y):-
    X=:=X2,
    Y=:=Y2,!.

cFichaMasAltaR2([],_X,_Y,_FichaX,_FichaY).

cFichaMasAltaR1(Lista,[X,Y|_Z],FichaX2,FichaY2):-
    cFichaMasAltaR2(Lista,X,Y,FichaX,FichaY),
    number(FichaX),
    FichaX2 is FichaX,
    FichaY2 is FichaY,!.

cFichaMasAltaR1(Lista,[_X,_Y|Z],FichaX,FichaY):-
    cFichaMasAltaR1(Lista,Z,FichaX,FichaY).


cFichaMasAlta(FichaX,FichaY):-
    Lista = [6,6,5,5,4,4,3,3,2,2,1,1,0,0,6,5,6,4,6,3,6,2,6,1,6,0,5,4,5,3,5,2,5,1,5,0,4,3,4,2,4,1,4,0,3,2,3,1,3,0,2,1,2,0,1,0],
    fichasJugador(ListaJ,_,1),
    cFichaMasAltaR1(ListaJ,Lista,FichaX,FichaY).

cEmpieza:-
    cFichaMasAlta(FichaX,FichaY),
    record(1,1,FichaX,FichaY,-1,1,1),
    nl,write("Ficha: "),
    write(FichaX),
    write("-"),
    write(FichaY),!.

%limite(i,o)
%Método para elegir la profundidad
limite(Cont,Cont):-
    Cont < 11,!.

limite(_,11).

%tirada(i,i,i,i,i,i,i,o,o,o,o)
%Método para agregar la ficha dada por minimax
tirada(ListaP,X,_Y,_FichaDondeSeColoco,Tirada1,Tirada2,ListaT,ListaT,ListaP,Tirada1,Tirada2):-
    not(number(X)),
    nl,write("Debo comer"),!.

tirada(ListaP,_X,Y,_FichaDondeSeColoco,Tirada1,Tirada2,ListaT,ListaT,ListaP,Tirada1,Tirada2):-
    not(number(Y)),
    nl,write("Debo comer"),!.

tirada(ListaP,_X,_Y,FichaDondeSeColoco,Tirada1,Tirada2,ListaT,ListaT,ListaP,Tirada1,Tirada2):-
    not(number(FichaDondeSeColoco)),
    nl,write("Debo comer"),!.

tirada(ListaP,X,Y,FichaDondeSeColoco,Tirada1,Tirada2,ListaT,ListaT2,ListaP2,NTirada1,NTirada2):-
    record2(ListaP,X,Y,FichaDondeSeColoco,Tirada1,Tirada2,ListaT,ListaT2,ListaP2,NTirada1,NTirada2),
    nl,
    write("Ficha: "),
    write(X),
    write(" - "),
    write(Y),
    write(" en la posición de "),
    write(FichaDondeSeColoco).

%imprimeFichasR(i,i,o)
%Método recursivo que imprime la lista
imprimeFichasR([X,Y|Z],Cont,Cant):-
    Cont<Cant,
    write(X),
    write("-"),
    write(Y),nl,
    Aux is Cont +1,
    imprimeFichasR(Z,Aux,Cant).

imprimeFichasR([],_,_).

%Muestra las fichas jugadas, la cantidad y la siguiente tirada
imprimeFichasJugadas:-
    fichasJugadas(Lista,Cant,[Tirada1,Ant1],[Tirada2,Ant2],1),
    imprimeFichasR(Lista,0,Cant),
    write("Cantidad: "),
    write(Cant),nl,
    write("Tirada1: "),
    write(Tirada1),write(" Antigüedad: "),write(Ant1),nl,
    write("Tirada2: "),
    write(Tirada2),write(" Antigüedad: "),write(Ant2),!.

%imprimeFichasJugador(i)
%1 pc, 2 oponente
%Imprime la lista de fichas del jugador y la cantidad
imprimeFichasJugador(NumJugador):-
    fichasJugador(Lista,Cant,NumJugador),
    imprimeFichasR(Lista,0,Cant),
    write("Cantidad: "),
    write(Cant),!.














