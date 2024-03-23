% Titulo:  Mundo de bloques
% Fecha:   sáb 23 mar 2024 01:52:45
% 
% Autores:  
%   Luis Eduardo Galindo Amaya
%   
% Ejemplo consulta:
%   resolver([3,4,1,2],[1,2,3,4]).



% print_list(Lista). 
print_list([]) :- nl, !.
print_list([X|Xs]) :- write(X), write(' '), print_list(Xs).

% conc(L1, L2). 
conc([],L,L) :- !.
conc([X|R],L,[X|Q]):- conc(R,L,Q).

% inverso(L1, R).
inverso([],[]) :- !.
inverso([X|R], L) :- inverso(R, Q), conc(Q, [X], L).

% elemento(X, L).
elemento(X,[X|_]) :- !.
elemento(X,[_|R]) :- elemento(X,R).



% resolver(Elementos, Orden).
resolver(A, Z) :-
    inverso(Z, L),
    print_list([A,[],[],[]]),
    solucion([A,[],[],[]], L).


solucion(_, []) :- !.           % si siguiente esta vacio 

solucion([[A | As], B, C, D], [A | T]) :-
    % si el primer elemento de 'A' es el siguiente
    Operacion = [As, B, C, [A | D]], 
    print_list(Operacion),
    solucion(Operacion, T).


solucion([A, [B | Bs], C, D], [B | T]) :-
    % si el primer elemento de 'B' es el siguiente
    Operacion = [A, Bs, C, [B|D]],
    print_list(Operacion),
    solucion(Operacion, T).


solucion([A, B, [C | Cs], D], [C | T]) :-
    % si el primer elemento de 'C' es el siguiente
    Operacion = [A, B, Cs, [C|D]],
    print_list(Operacion),
    solucion(Operacion, T).


solucion([[A|As],B,C,D], [R|Rs]) :-
    % si siguiente esta el la pila A
    elemento(R, As),
    Operacion = [As,[A|B],C,D],
    print_list(Operacion),
    solucion(Operacion, [R|Rs]).

solucion([A,[B|Bs],C,D], [R|Rs]) :-
    % si siguiente esta el la pila B
    elemento(R, Bs),
    Operacion = [A,Bs,[B|C],D],
    print_list(Operacion),
    solucion(Operacion, [R|Rs]).

solucion([A,B,[C|Cs],D], [R|Rs]) :-
    % si siguiente esta el la pila C
    elemento(R, Cs),
    Operacion = [[C|A],B,Cs,D],
    print_list(Operacion),
    solucion(Operacion, [R|Rs]).
