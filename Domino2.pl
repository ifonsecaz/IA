:- dynamic fichasJugador/3. %Id 1 es la computadora, 2 el otro. (o 1 quien empieza)
:- dynamic fichasJugadas/5.
:- retractall(fichasJugadas(_,_,_,_,_)). %Id 1 es el tablero actual
:- assert(fichasJugadas([],0,-1,-1,1)). %c cualquier ficha se puede jugar
:- retractall(fichasJugador(_,_,_)).
:- assert(fichasJugador([],0,1)).
:- assert(fichasJugador([],0,2)).
:- dynamic contador/2.
:- assert(contador(0,1)).

mark(Player,Position,X,Y,PosicionFicha) :-
    fichasJugador(Lista1,_,Player),
    fichasJugadas(_,_,Tirada1,Tirada2,Position),
    %write("buscando"),
    busca(Lista1,Tirada1,Tirada2,X,Y,PosicionFicha).
    %write("Termine").

win(Player,1):-
    fichasJugador([],_,Player),!.

win(Player,0):- %Todavía no gana
    fichasJugador(_,_,Player).


%busca([],_,_,-1,-1,1):-write("Entre"),!. %Debe comer

busca([X,Y|_],X,_,X,Y,X).

busca([X,Y|_],Y,_,X,Y,Y).

busca([X,Y|_],Aux,X,X,Y,X):-
    Aux =\=X.
  %Evita repetir
busca([X,Y|_],Aux,Y,X,Y,Y):-
    Aux =\=Y.
busca([_,_|Z],X,Y,X1,Y1,PosFicha):-
    busca(Z,X,Y,X1,Y1,PosFicha).

posiblesFichas(NumJugador):-
    Lista = [6,6,6,5,6,4,6,3,6,2,6,1,6,0,5,5,5,4,5,3,5,2,5,1,5,0,4,4,4,3,4,2,4,1,4,0,3,3,3,2,3,1,3,0,2,2,2,1,2,0,1,1,1,0,0,0],
    fichasJugador(FichasJugador1,Cont,1),
    fichasJugadas(FichasJugadas,Cont2,_,_,1),
    descartaPosiblesFichas(FichasJugadas,Lista,0,Cont2,ListaR),
    descartaPosiblesFichas(FichasJugador1,ListaR,0,Cont,ListaR2),
    Aux is 28- Cont - Cont2,
    retract(fichasJugador(_,_,NumJugador)),
    asserta(fichasJugador(ListaR2,Aux,NumJugador)).

descartaPosiblesFichas([X,Y|Z],Lista2,Cont,Lim,Res):-
    Cont<Lim,
    elimina(X,Y,Lista2,NLista),
    Aux is Cont+1,
    descartaPosiblesFichas(Z,NLista,Aux,Lim,Res).

descartaPosiblesFichas([],Res,Lim,Lim,Res).

% Solo se descartan las fichas que no tenía, antes se debe llamar a
% posiblesFichas
oponenteComio(Ficha1,Ficha2):-
    retract(fichasJugador(Lista,Cant,2)),
    descartaNumeroR(Lista,0,Ficha1,Total,ListaR),
    descartaNumeroR(ListaR,Total,Ficha2,Total2,ListaR2),
    Aux is Cant - Total2,
    asserta(fichasJugador(ListaR2,Aux,2)).

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


agrega(X,Y,[],[X,Y]):-!.
/*    nl,
    write("agregó: "),
    write(X),
    write(Y),
    nl.
*/
agrega(X,Y,[H1,H2|T],[H1,H2|Res]):-
    agrega(X,Y,T,Res).

elimina( _,_, [], []).
elimina(X,Y, [X,Y|T], T).
elimina(X,Y, [H1,H2|T], [H1,H2|T2]) :-
    H1 =\= X,
    elimina(X,Y, T, T2).

elimina(X,Y, [X,H2|T], [X,H2|T2]) :-
    H2 =\= Y,
    elimina(X,Y, T, T2).


sigTirada(X,Y,_,-1,-1,X,Y):-
    !.

sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,Y,Tirada2):-
    FichaDondeSeColoco =:= Tirada1,
    X =:= Tirada1,!.

sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,Y,Tirada1):-
    FichaDondeSeColoco =:= Tirada2,
    X =:= Tirada2,!.

sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,X,Tirada2):-
    FichaDondeSeColoco =:= Tirada1,
    Y =:= Tirada1,!.

sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,X,Tirada1):-
    FichaDondeSeColoco =:= Tirada2,
    Y =:= Tirada2,!.

record(Player,Player2,X,Y,FichaDondeSeColoco,Tablero,Tablero2) :-
   fichasJugadas(Lista,Cant,Tirada1,Tirada2,Tablero),
   agrega(X,Y,Lista,Lista2),
   %nl,write("Fichas Jugadas"),
   %write(Lista2),
   sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,NTirada1,NTirada2),
   %nl,
   %write("SigTirada:"),
   %write(NTirada1),
   %write(" o "),
   %write(NTirada2),
   Aux is Cant + 1,
   asserta(fichasJugadas(Lista2,Aux,NTirada1,NTirada2,Tablero2)),
   fichasJugador(ListaJ,Cant2,Player),
   Aux2 is Cant2 -1,
   elimina(X,Y,ListaJ,ListaJ2),
   asserta(fichasJugador(ListaJ2,Aux2,Player2)).

%Caso PosJugador par si tiene que comer o ganó
get_pos_value(PosJugador,_PosTablero,Val):-
    fichasJugador(ListaJ,Cant,PosJugador),
    %fichasJugadas(ListaT,CantT,PosTablero),
    Aux is Cant *(-5),
    winPos(Cant,Aux2),
    mulas(ListaJ,0,CantM),
    Aux3 is CantM*(-10),
    Val is Aux + Aux2+Aux3,
    retract(contador(Cont,1)),
    AuxC is Cont +1,
    assert(contador(AuxC,1)).
    %nl,
    %write("get"),
    %retract(contador(Cont,1)),
    %Aux is Cont +1,
    %asserta(contador(Aux,1)).
/*
get_pos_value(_,_,10):-
    retract(contador(Cont,1)),
    Aux is Cont +1,
    assert(contador(Aux,1)).
*/
winPos(0,100):-!.

winPos(_,0).

mulas([],Cont,Cont):-!.

mulas([X,X|Z],Cont,CantM):-
    Aux is Cont + 1,
    mulas(Z,Aux,CantM).

mulas([X,Y|Z],Cont,CantM):-
    X =\= Y,
    mulas(Z,Cont,CantM).


moves(Pos,PosList,Tiradas):-
   findall([X,Y,PosicionFicha],mark(Pos, PosList,X,Y,PosicionFicha),Tiradas),!.
/*
alphabeta(Pos, PosList,_, _Beta, _GoodPos, Val) :-
   %findall([X,Y,PosicionFicha],mark(Pos, PosList,X,Y,PosicionFicha),Tiradas),!,
   moves(Pos,PosList,[]),!,
   %boundedbest( Tiradas,Pos,PosList, Alpha, Beta, GoodPos, Val);
   get_pos_value(Pos,Val).
*/
alphabeta(Pos, PosTablero,_,_,_, Val,Depth) :-
   Depth > 5,
   %nl,
   %write("Llegó al final"),
   get_pos_value(Pos,PosTablero,Val),!.

alphabeta(Pos, PosTablero,_,_,_, Val,_) :-
   moves(Pos,PosTablero,[]),
   %nl,
   %write("Debo comer"),
   get_pos_value(Pos,PosTablero,Val),!.

alphabeta(Pos, PosList,Alpha, Beta, GoodPos, Val,Depth) :-
   %nl,
    %write("bajando"),
    Depth1 is Depth +1,
   %findall([X,Y,PosicionFicha],mark(Pos, PosList,X,Y,PosicionFicha),Tiradas),!,
   moves(Pos,PosList,Tiradas),
   boundedbest( Tiradas,Pos,PosList, Alpha, Beta, GoodPos, Val,Depth1),!.

boundedbest([[X,Y,PosFicha]|Moves],PosJugador,PosTablero, Alpha, Beta, GoodPos, GoodVal,Depth) :- %Saco un movimiento
   %nl,
   %write("Saca un movimiento"),
   %desencola(Tiradas,X,Y,PosFicha),
   Aux is PosTablero + 1, %Nuevo tablero
   Aux2 is PosJugador +2, %Nuevo Jugador
   record(PosJugador,Aux2,X,Y,PosFicha,PosTablero,Aux),
   other_player(PosJugador,JugadorSig), %Sig jugador
   alphabeta(JugadorSig,Aux, Alpha, Beta, _, Val,Depth),
   %nl,write("goodEnough"),
   goodenough(PosJugador,[X,Y,PosFicha],Moves, Alpha, Beta,PosTablero,Val, GoodPos, GoodVal,Depth).
   %nl,
   %write(GoodPos).

goodenough(_,PosList,[],_,_,_, Val, PosList, Val,_) :- !.               % no other candidate

goodenough(PosJugador,PosList,_, _, Beta, _, Val, PosList, Val,_) :-
    min_to_move(PosJugador), Val > Beta,
  %  nl,
  % write(Val),
 %  write(" > "),
%   write(Beta),
!.         % Maximizer attained upper bound

     % Minimizer attained lower bound
goodenough(PosJugador,PosList,_, Alpha, _,_, Val, PosList, Val,_) :-
   max_to_move(PosJugador), Val < Alpha,    %nl,
 %  write(Val),
 %  write(" < "),
 %  write(Alpha),
!.  %Max_to_move solo define si estoy minimizando o maximizando debe dar true

goodenough( PosJugador,PosList,Moves, Alpha, Beta, PosTablero, Val, GoodPos, GoodVal,Depth) :-
   newbounds( Alpha, Beta, PosJugador, Val, NewAlpha, NewBeta),  % Refine bounds
   %nl,
   %write("Newbounds"),
   %nl,
   %write("PosJugador: "),
   %write(PosJugador),
   %retract(fichasJugador(_,_,PosJugador)),
   boundedbest(Moves,PosJugador,PosTablero, NewAlpha, NewBeta, Pos1, Val1,Depth), %Aquí evalua el resto
   betterof( PosJugador,PosList, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
   min_to_move(Pos), Val > Alpha,
      %nl,
%write("Newbounds "),
 %     write(Val),
 %  write(" > "),
 %  write(Alpha),
!.          % Mazximizer increased lower bound

newbounds(Alpha, Beta, Pos, Val, Alpha, Val):-
   max_to_move(Pos), Val < Beta,
 %     nl,
%      write("newbounds "),
%   write(Val),
%   write(" < "),
%   write(Beta),
!.            % Minimizer decreased upper bound

newbounds( Alpha, Beta, _,_,Alpha, Beta).    % otherwise bounds unchanged.

betterof(PosJugador,PosList, Val, _, Val1, PosList, Val) :-          % Pos better than Pos1
   min_to_move(PosJugador), Val > Val1, !.

betterof(PosJugador,PosList, Val, _, Val1, PosList, Val) :-          % Pos better than Pos1
   max_to_move(PosJugador), Val < Val1, !.

betterof(_,_,_,Pos1,Val1,Pos1,Val1).                % otherwise Pos 1 better

min_to_move(Pos):-
    Aux is Pos mod 2,
    Aux =:= 0.

max_to_move(Pos):-
    Aux is Pos mod 2,
    Aux =:= 1.



other_player(Jugador,SigJugador):-
    SigJugador is Jugador +1.

desencola([Move],Move).
desencola([_|Resto],Last):-
    last(Resto,Last).
desencola(Move,Move).

move(4,[_,-1,-1],PositionTablero,PositionTablero,4). %Contrincante come o pasa

move(3,[_,-1,-1],PositionTablero,PositionTablero,3).

move(Player,[Pos,X,Y],PositionTablero,PositionTablero1,0):-
    %write("Move"),
    %write(X),
    %write(Y),
    PositionTablero1 is PositionTablero+1,
    record(Player,Player,X,Y,Pos,PositionTablero,PositionTablero1).


value(_,_,Value):-
    Value is 10.


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

fichasNuevoJuego(NumJugador):-
    fichasNuevoJuegoR(1,NumJugador),
    posiblesFichas(2).

oponenteCome(Ficha1,Ficha2):-
    posiblesFichas(2),
    oponenteComio(Ficha1,Ficha2).

oponentePaso(Ficha1,Ficha2):-
    oponenteComio(Ficha1,Ficha2).

comer(NumJugador):-
    nl,
    write("Dame la siguiente ficha"),
    nl,
    read(X),
    read(Y),
    retract(fichasJugador(ListaJ,Cant,NumJugador)),
    agrega(X,Y,ListaJ,ListaJ2),
    Cont is Cant +1,
    asserta(fichasJugador(ListaJ2,Cont,NumJugador)).


jugada(FichaX,FichaY,Pos,NumJugador):-
    record(NumJugador,NumJugador,FichaX,FichaY,Pos,1,1).

jugada2(FichaX,FichaY,Pos):-
   retract(fichasJugadas(Lista,Cant,Tirada1,Tirada2,1)),
   agrega(FichaX,FichaY,Lista,Lista2),
   sigTirada(FichaX,FichaY,Pos,Tirada1,Tirada2,NTirada1,NTirada2),
   Aux is Cant + 1,
   asserta(fichasJugadas(Lista2,Aux,NTirada1,NTirada2,1)).

c(FichaX,FichaY,Pos):-
   %retract(fichasJugadas(Lista,Cant,Tirada1,Tirada2,1)),
   fichasJugadas(Lista,Cant,Tirada1,Tirada2,1),
   asserta(fichasJugadas(Lista,Cant,Tirada1,Tirada2,2)), %Mesa virtual
   fichasJugador(ListaJ2,Cont,1),
   asserta(fichasJugador(ListaJ2,Cont,3)), %Maquina fichas virtuales
   fichasJugador(ListaJ3,Cont2,2),
   asserta(fichasJugador(ListaJ3,Cont2,4)), %Oponente fichas virtuales
   %Depth is 28-Cant,
   Depth is 1,
   %write("Entra a alpha"),!,
   alphabeta(3,2,-200,200,[FichaX,FichaY,Pos],Value,Depth), %28 de profundidad
   write(Value),
   agrega(FichaX,FichaY,Lista,Lista2),
   elimina(FichaX,FichaY,ListaJ2,NLista),
   sigTirada(FichaX,FichaY,Pos,Tirada1,Tirada2,NTirada1,NTirada2),
   contador(Contttt,1),
   write("Comp "),
   write(Contttt),
   Aux is Cant + 1,
   Aux2 is Cant-1,
   retractall(fichasJugadas(_,_,_,_,_)),
   retractall(fichasJugador(_,_,_)),
   asserta(fichasJugador(NLista,Aux2,1)),
   asserta(fichasJugador(ListaJ3,Cont2,2)),
   asserta(fichasJugadas(Lista2,Aux,NTirada1,NTirada2,1)).


imprimeFichasR([X,Y|Z],Cont,Cant):-
    Cont<Cant,
    write(X),
    write("-"),
    write(Y),nl,
    Aux is Cont +1,
    imprimeFichasR(Z,Aux,Cant).

imprimeFichasR([],_,_).

imprimeFichasJugadas:-
    fichasJugadas(Lista,Cant,Tirada1,Tirada2,1),
    imprimeFichasR(Lista,0,Cant),
    write("Cantidad: "),
    write(Cant),nl,
    write("Tirada1: "),
    write(Tirada1),nl,
    write("Tirada2: "),
    write(Tirada2).

imprimeFichasJugador(NumJugador):-
    fichasJugador(Lista,Cant,NumJugador),
    imprimeFichasR(Lista,0,Cant),
    write("Cantidad: "),
    write(Cant).

















