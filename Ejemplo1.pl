/* �ste es un comentario
   multi-l�nea. */
% �ste es un comentario de una sola l�nea.

hombre(jose).
hombre(juan).
mujer(maria).
papa(juan,jose).
papa(juan,maria).
valioso(dinero).
dificilDeObtener(dinero).
le_da(pedro,libro,antonio).

hermana(X,Y):-
    papa(Z,X),
    mujer(X),
    papa(Z,Y),
    X\==Y.

hijo(X,Y):-
    papa(Y,X),
    hombre(X).

/*
humano(X):-
    mujer(X);
    hombre(X).
*/

humano(X):-mujer(X).
humano(X):-hombre(X).




