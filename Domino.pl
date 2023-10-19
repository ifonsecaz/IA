:- dynamic fichasJugador/2. %Id 1 es la computadora, 2 el otro. (o 1 quien empieza)
:- dynamic fichasJugadas/4.
:- retractall(fichasJugadas(_,_,_,_)). %Id 1 es el tablero actual
:- assert(fichasJugadas([_,_],c,c,1)). %c cualquier ficha se puede jugar

% Fichas 6,6 6,5 6,4 6,3 6,2 6,1 6,0 5,5 5,4 5,3 5,2 5,1 5,0 4,4 4,3 4,2
% 4,1 4,0 3,3 3,2 3,1 3,0 2,2 2,1 2,0 1,1 1,0 0,0
%
% Position lleva fichasJugadas con Id 2

%%%%%
%%  Generate possible marks on a free spot on the board.
%%  Use mark(+,+,-X,-Y) to query/generate possible moves (X,Y).
%%%%% Posibles tiradas
mark(Player,Position,X,Y,Come) :-
    fichasJugador(Lista1,Player),
    fichasJugadas(_,Tirada1,Tirada2,Position),
    busca(Lista1,Tirada1,Tirada2,X,Y,Come).

busca([],_,_,c,c,1). %Debe comer
busca([X,Y|_],X,_,X,Y,0).
busca([X,Y|_],Y,_,X,Y,0).
busca([X,Y|_],_,X,X,Y,0).
busca([X,Y|_],_,Y,X,Y,0).
busca([_,_|Z],X,Y,X1,Y1,Come):-
    busca(Z,X,Y,X1,Y1,Come).



%mark(Player, [_,X|_],2,1) :- var(X), X=Player.
%mark(Player, [_,_,X|_],3,1) :- var(X), X=Player.
%mark(Player, [_,_,_,X|_],1,2) :- var(X), X=Player.
%mark(Player, [_,_,_,_,X|_],2,2) :- var(X), X=Player.
%mark(Player, [_,_,_,_,_,X|_],3,2) :- var(X), X=Player.
%mark(Player, [_,_,_,_,_,_,X|_],1,3) :- var(X), X=Player.
%mark(Player, [_,_,_,_,_,_,_,X|_],2,3) :- var(X), X=Player.
%mark(Player, [_,_,_,_,_,_,_,_,X|_],3,3) :- var(X), X=Player.
agrega(X,Y,[],[X,Y]).

agrega(X,Y,[H1,H2|T],[H1,H2|Res]):-
    agrega(X,Y,T,Res).

elimina( _,_, [], []).
elimina(X,Y, [X,Y|T], T).
elimina(X,Y, [H1,H2|T], [H1,H2|T2]) :-
    H1 =\= X,
    H2 =\= Y,
    elimina(X,Y, T, T2).

sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,Y,Tirada2):-
    FichaDondeSeColoco =:= Tirada1,
    X =:= Tirada1.

sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,Y,Tirada1):-
    FichaDondeSeColoco =:= Tirada2,
    X =:= Tirada2.

sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,X,Tirada2):-
    FichaDondeSeColoco =:= Tirada1,
    Y =:= Tirada1.

sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,X,Tirada1):-
    FichaDondeSeColoco =:= Tirada2,
    Y =:= Tirada2.


%%%%%
%%  Record a final move: record(+,+,+). %Las fichas se dan con el valor
%%  mayor a menor
%%  %
record(Player,X,Y,FichaDondeSeColoco) :-
   retract(fichasJugadas(Lista,Tirada1,Tirada2,1)),
   agrega(X,Y,Lista,Lista2),
   sigTirada(X,Y,FichaDondeSeColoco,Tirada1,Tirada2,NTirada1,NTirada2),
   assert(fichasJugadas(Lista2,NTirada1,NTirada2,1)),
   retract(fichasJugador(ListaJ,Player)),
   elimina(ListaJ,X,Y,ListaJ2),
   assert(fichasJugador(ListaJ2,Player)).


%%%%%
%%  A winning line is ALREADY bound to Player.
%%  win(+Board,+Player) is true or fail.
%%    e.g., win([P,P,P|_],P).  is NOT correct, because could bind
%%%%%
%win([Z1,Z2,Z3|_],P) :- Z1==P, Z2==P, Z3==P.
%win([_,_,_,Z1,Z2,Z3|_],P) :-  Z1==P, Z2==P, Z3==P.
%win([_,_,_,_,_,_,Z1,Z2,Z3],P) :-  Z1==P, Z2==P, Z3==P.
%win([Z1,_,_,Z2,_,_,Z3,_,_],P) :-  Z1==P, Z2==P, Z3==P.
%win([_,Z1,_,_,Z2,_,_,Z3,_],P) :-  Z1==P, Z2==P, Z3==P.
%win([_,_,Z1,_,_,Z2,_,_,Z3],P) :-  Z1==P, Z2==P, Z3==P.
%win([Z1,_,_,_,Z2,_,_,_,Z3],P) :-  Z1==P, Z2==P, Z3==P.
%win([_,_,Z1,_,Z2,_,Z3,_,_],P) :-  Z1==P, Z2==P, Z3==P.

win(Player,1):-
    fichasJugador([],Player),!.

win(Player,0):- %Todavía no gana
    fichasJugador(_,Player).


%%%%%
%%  A line is open if each position is either free or equals the Player
%%%%%
/*
open([Z1,Z2,Z3|_],Player) :- (var(Z1) | Z1 == Player),(var(Z2) | Z2 == Player), (var(Z3) | Z3 == Player).
open([_,_,_,Z1,Z2,Z3|_],Player) :- (var(Z1) | Z1 == Player),(var(Z2) | Z2 == Player), (var(Z3) | Z3 == Player).
open([_,_,_,_,_,_,Z1,Z2,Z3],Player) :- (var(Z1) | Z1 == Player),(var(Z2) | Z2 == Player), (var(Z3) | Z3 == Player).
open([Z1,_,_,Z2,_,_,Z3,_,_],Player) :- (var(Z1) | Z1 == Player),(var(Z2) | Z2 == Player), (var(Z3) | Z3 == Player).
open([_,Z1,_,_,Z2,_,_,Z3,_],Player) :- (var(Z1) | Z1 == Player),(var(Z2) | Z2 == Player), (var(Z3) | Z3 == Player).
open([_,_,Z1,_,_,Z2,_,_,Z3],Player) :- (var(Z1) | Z1 == Player),(var(Z2) | Z2 == Player), (var(Z3) | Z3 == Player).
open([Z1,_,_,_,Z2,_,_,_,Z3],Player) :- (var(Z1) | Z1 == Player),(var(Z2) | Z2 == Player), (var(Z3) | Z3 == Player).
open([_,_,Z1,_,Z2,_,Z3,_,_],Player) :- (var(Z1) | Z1 == Player),(var(Z2) | Z2 == Player), (var(Z3) | Z3 == Player).
*/

move(Player,Move,Position,Position1). %Problema si una ficha se puede tirar en 2 posiciones


%%%%%
%% Calculate the value of a position, o maximizes, x minimizes.
%%%%%
value(Board,100) :- win(Board,o), !.
value(Board,-100) :- win(Board,x), !.
value(Board,E) :-
   findall(o,open(Board,o),MAX),
   length(MAX,Emax),      % # lines open to o
   findall(x,open(Board,x),MIN),
   length(MIN,Emin),      % # lines open to x
   E is Emax - Emin.

alpha_beta(_,0,Position,_Alpha,_Beta,_NoMove,Value) :-
   value(Position,Value).

alpha_beta(Player,D,Position,Alpha,Beta,Move,Value) :-
   D > 0,
   findall([X,Y],mark(Player,Position,X,Y),Moves), %Guarda en la lista moves los X,Y, que coinciden con la condicion de mark
   Alpha1 is -Beta, % max/min
   Beta1 is -Alpha,
   D1 is D-1,
   evaluate_and_choose(Player,Moves,Position,D1,Alpha1,Beta1,nil,(Move,Value)).

evaluate_and_choose(Player,[Move|Moves],Position,D,Alpha,Beta,Record,BestMove) :-
   move(Player,Move,Position,Position1),
   other_player(Player,OtherPlayer),
   alpha_beta(OtherPlayer,D,Position1,Alpha,Beta,_OtherMove,Value),
   Value1 is -Value,
   cutoff(Player,Move,Value1,D,Alpha,Beta,Moves,Position,Record,BestMove).

evaluate_and_choose(_Player,[],_Position,_D,Alpha,_Beta,Move,(Move,Alpha)).

cutoff(_Player,Move,Value,_D,_Alpha,Beta,_Moves,_Position,_Record,(Move,Value)) :-
   Value >= Beta, !.
cutoff(Player,Move,Value,D,Alpha,Beta,Moves,Position,_Record,BestMove) :-
   Alpha < Value, Value < Beta, !,
   evaluate_and_choose(Player,Moves,Position,D,Value,Beta,Move,BestMove).
cutoff(Player,_Move,Value,D,Alpha,Beta,Moves,Position,Record,BestMove) :-
   Value =< Alpha, !,
   evaluate_and_choose(Player,Moves,Position,D,Alpha,Beta,Record,BestMove).

other_player(o,x).
other_player(x,o).

h(X,Y) :- record(x,X,Y), showBoard.

c :-
   fichasJugadas(Lista,Tirada1,Tirada2,1),
   asserta(fichasJugadas(Lista,Tirada1,Tirada2,2)),
   alpha_beta(o,2,2,-200,200,(X,Y),_Value), % <=== NOTE
   retract(fichasJugadas(_,_,_,2)),
   record(o,X,Y), showBoard.

showBoard :-
   board([Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9]),
   write('    '),mark(Z1),write(' '),mark(Z2),write(' '),mark(Z3),nl,
   write('    '),mark(Z4),write(' '),mark(Z5),write(' '),mark(Z6),nl,
   write('    '),mark(Z7),write(' '),mark(Z8),write(' '),mark(Z9),nl.
s :- showBoard.

mark(X) :- var(X), write('#').
mark(X) :- \+var(X),write(X).

