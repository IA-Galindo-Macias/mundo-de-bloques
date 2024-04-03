?-retractall(state(|_)),
  window( title("Mundo de Bloques"), size(600, 300)).


win_func(init) :-   
	menu(normal, action(menu_run), text("&Ejecutar") ),
	pen(2, rgb(255,255,255)),
        window_brush(_, rgb(255, 255, 255)).

win_func(paint) :-	reset.

unvisible:-brush(rgb(255,255,255)).
visible:-brush(rgb(255,0,0)).

verde:- brush(rgb(0, 255, 0)).
rojo:- brush(rgb(255, 0, 0)).
azul:- brush(rgb(0, 0, 255)).
magenta:- brush(rgb(255, 0, 255)).



menu_run(press):-resolver([3,4,1,2],[1,2,3,4]).

reset:-
	show_stack([3,4,1,2]),
	retract(state(_,_,_,_)).


% resolver(Elementos, Orden).
resolver(A, Z) :-
	inverso(Z, L),
	assert(state(A,[],[],[])),
	solucion([A,[],[],[]], L).

solucion(_, []) :- !.

solucion([[A | As], B, C, D], [A | T]) :-
    Operacion = [As, B, C, [A | D]], 
    inform(posUno, posCuatro),
    solucion(Operacion, T).


solucion([A, [B | Bs], C, D], [B | T]) :-
    Operacion = [A, Bs, C, [B|D]],
    inform(posDos, posCuatro),
    solucion(Operacion, T).


solucion([A, B, [C | Cs], D], [C | T]) :-
    Operacion = [A, B, Cs, [C|D]],
    inform(posTres, posCuatro),
    solucion(Operacion, T).


solucion([[A|As],B,C,D], [R|Rs]) :-
    elemento(R, As),
    Operacion = [As,[A|B],C,D],
    inform(posUno, posDos),
    solucion(Operacion, [R|Rs]).

solucion([A,[B|Bs],C,D], [R|Rs]) :-
    elemento(R, Bs),
    Operacion = [A,Bs,[B|C],D],
    inform(posDos, posTres),
    solucion(Operacion, [R|Rs]).

solucion([A,B,[C|Cs],D], [R|Rs]) :-
	elemento(R, Cs),
	Operacion = [[C|A],B,Cs,D],
	inform(posTres, posUno),
	print_list(Operacion),
	solucion(Operacion, [R|Rs]).





inform(posUno, posCuatro) :- state([X|PosUno], PosDos, PosTres, PosCuatro),
	retract(state(_,_,_,_)),
	write(X), write(" Primero->Cuarto"), nl,
	write(PosUno),write(PosCuatro), nl,
	showmv(X,PosUno,100,PosCuatro,400),
	assert(state(PosUno, PosDos, PosTres, [X|PosCuatro])).


inform(posDos, posCuatro) :- state(PosUno, [X|PosDos], PosTres, PosCuatro),
	retract(state(_,_,_,_)),
	write(X), write(" Segundo->Cuarto"), nl,
	write(PosUno),write(PosCuatro), nl,
	showmv(X,PosDos,200,PosCuatro,400),
	assert(state(PosUno, PosDos, PosTres, [X|PosCuatro])).


inform(posTres, posCuatro) :- state(PosUno, PosDos, [X|PosTres], PosCuatro),
	retract(state(_,_,_,_)),
	write(X), write(" Tercero->Cuarto"), nl,
	write(PosUno),write(PosCuatro), nl,
	showmv(X,PosTres,300,PosCuatro,400),
	assert(state(PosUno, PosDos, PosTres, [X|PosCuatro])).


inform(posUno, posDos) :- state([X|PosUno], PosDos, PosTres, PosCuatro),
	retract(state(_,_,_,_)),
	write(X), write(" Primero->Segundo"), nl,
	write(PosUno),write(PosCuatro), nl,
	showmv(X,PosUno,100,PosDos,200),
	assert(state(PosUno, [X|PosDos], PosTres, PosCuatro)).


inform(posDos, posTres) :- state(PosUno, [X|PosDos], PosTres, PosCuatro),
	retract(state(_,_,_,_)),
	write(X), write(" Segundo->Tercero"), nl,
	write(PosUno),write(PosCuatro), nl,
	showmv(X,PosDos,200,PosTres,300),
	assert(state(PosUno, PosDos, [X|PosTres], PosCuatro)).


inform(posTres, posUno) :- state(PosUno, PosDos, [X|PosTres], PosCuatro),
	retract(state(_,_,_,_)),
	write(X), write(" Tercero->Primero"), nl,
	write(PosUno),write(PosCuatro), nl,
	showmv(X,PosTres,300,PosUno,100),
	assert(state([X|PosUno], PosDos, PosTres, PosCuatro)).



make_list(N, N, [N]) :- ! .
make_list(N, X, [X|T]) :-  
	 X1:=X + 1,
         make_list(N, X1, T).

show_stack([]) :- ! .
show_stack([X|L]):-
	height(L, H),
	colores(Cs),
	elementoN(X,Cs,1,C),
	C,
	box(X, 100, H),
	%free(C),
	show_stack(L).

height(L,H):-N is list_length(L),
	     H:=200 - N*20.
	

wrLists(L,M,R):-write(L),
                write(M),
	        write(R),nl.

movBox(X,X1,Y1,X2,Y2):-moveBox(X,X1,Y1,X2,Y2), ! .

showmv(X,_,X1,_,X1):- ! .
showmv(X,L1,X1,L2,X2):-
  height(L1,H1),height(L2,H2),
  movBox(X,X1,H1,X1,20),
  movBox(X,X1,20,X2,20),
  movBox(X,X2,20,X2,H2).

box(Size,X,Y):-	
	X1:=X - Size*10,
	X2:=X + Size*10,
	Y1:=Y - 20,
	rect(X1,Y,X2,Y1).
	

getstep(0,X,X) :- ! .
getstep(10,X,Y):-Y>X, ! .
getstep(-10,X,Y):-Y<X, ! .


moveBox(Size,X1,Y1,X2,Y2):-
	getstep(StepX,X1,X2),
	getstep(StepY,Y1,Y2),
	repeat,
	  unvisible, 
	  box(Size,X1,Y1),
	  X1:=X1+StepX, 
	  Y1:=Y1+StepY,
	  colores(Cs),
	  elementoN(Size,Cs,1,C),
	  C,
	  box(Size,X1,Y1),
	wait(0.02),
 	X1=X2, Y1=Y2.

   
fastmov(Size,X1,Y1,X2,Y2):-     
	  unvisible, box(Size,X1,Y1),
	  visible, box(Size,X2,Y2),
	  wait(0.1).


colores(L):- listaColores([azul, rojo, verde, magenta], L).

listaColores(N, N) :- ! .

elementoN(N, [E|_], N, E):-!.
elementoN(N, [_|R], I, E):- 
	I2:= I + 1, 
	elementoN(N, R, I2, E).

% conc(L1, L2). 
conc([],L,L) :- !.
conc([X|R],L,[X|Q]):- conc(R,L,Q).

% inverso(L1, R).
inverso([],[]) :- !.
inverso([X|R], L) :- inverso(R, Q), conc(Q, [X], L).

% elemento(X, L).
elemento(X,[X|_]) :- !.
elemento(X,[_|R]) :- elemento(X,R).
