write_list([]).

write_list([H|T]) :-
    writeln(H),
    write_list(T).

write_in_reverse([]).

write_in_reverse([H|T]) :-
    write_in_reverse(T),
    writeln(H).

seperate_list([], Atoms, Integers).

seperate_list([H|T], [H|Atoms], Integers) :-
    atom(H),
    seperate_list(T, Atoms, Integers).

seperate_list([H|T], Atoms, [H|Integers]) :-
    integer(H),
    seperate_list(T, Atoms, Integers).
