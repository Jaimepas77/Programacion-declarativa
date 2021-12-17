%Autor: Jaime Pastrana García
/*LAB7*/

%Probando parte 1...
/*elimina1([ ],_,[ ]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y, NR).
elimina1([X|R],Y,[X|NR]) :- Y \== X, elimina1(R,Y,NR).

elimina2([ ],X,[ ]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y, NR).
elimina2([X|R],Y,[X|NR]) :- Y \= X, elimina2(R,Y,NR).

elimina3([ ],X,[ ]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y \== X, elimina3(R,Y,NR).*/

%Parte 2 (para programar y entregar)

%1-
maximo(void, 0).
maximo(arbol(R, I, D), X):- maximo(I, Y), maximo(D, Z), 
							(R >= Y, R >= Z -> X is R; (Y > Z -> X is Y; X is Z)).

%2-
sublista([], []).
sublista([X|SXs], [X|Xs]):- sublistaIni(SXs, Xs).
sublista(SXs, [_|Xs]):- !, sublista(SXs, Xs).

%Auxiliar
sublistaIni([], _).
sublistaIni([X|SXs], [X|Xs]):- sublistaIni(SXs, Xs).

%Recolección de respuestas
sublistas(LSX, Xs):- findall(X, sublista(X, Xs), LSX).

%3-
aplana([], []).
aplana([[]|Xss], Ys):- !, aplana(Xss, Ys).
aplana([[X|Xs]|Xss], Rs):- !, aplana([X], S), append(S, Ys, Rs), aplana([Xs|Xss], Ys).
aplana([X|Xss], [X|Ys]):- !, aplana(Xss, Ys).%Caso de elemento que no es lista

%4-
hanoi(X, Origen, Destino, Auxiliar, [(Origen, Destino)]):- X =:= 1, !.
hanoi(N, Origen, Destino, Auxiliar, M):- !, Mov is N-1, 
										hanoi(Mov, Origen, Auxiliar, Destino, Rec),%Mover parte superior a la auxiliar
										append(Rec, [(Origen, Destino)], Tmp),%Mover disco inferior al destino
										hanoi(Mov, Auxiliar, Destino, Origen, Fin), append(Tmp, Fin, M).%Mover parte superior al destino