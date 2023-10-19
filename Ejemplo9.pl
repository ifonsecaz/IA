%combina(i,i,o), combina(i,i,i), combina(i,o,i), combina(o,i,i):
combina([],Lista,Lista):-!.
combina([X|Lista1],Lista2,[X|Lista3]):-
    combina(Lista1,Lista2,Lista3).

