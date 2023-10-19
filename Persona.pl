:- dynamic persona/3.

persona("Israel",2000,"H").


year(Year) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(year, DateTime, Year).

escribe_persona_y_edad(Nombre):-
    persona(Nombre,Year,Gen),
    write("Nombre "),
    write(Nombre),
    write(" Año de nacimiento "),
    write(Year),
    write(" Genero: "),
    write(Gen),
    write(" Edad "),
    year(Y),
    X is Y-Year,
    write(X).

main:-
    write("Dame el nombre"),
    nl,
    read(Nombre),
    nl,
    write("Dame el año de nacimiento"),
    nl,
    read(Nac),
    nl,
    write("Dame el genero"),
    nl,
    read(Gen),
    nl,
    asserta(persona(Nombre,Nac,Gen)).















