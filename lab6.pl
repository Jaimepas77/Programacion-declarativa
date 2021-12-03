%Comentario
%Autor: Jaime Pastrana García
%Lab6 (PD)

%Comentario corto
/*Comentario largo*/

%1
mas_por_encima_que(X,Y):- por_arriba_ls(X, Xs), por_arriba_ls(Y,Ys), mayor(Xs,Ys).

mayor([_|_], []).
mayor([_|Xs], [_|Ys]):- mayor(Xs, Ys).

%2
mezcla([], [], []).
mezcla([], L2s, L2s).
mezcla(L1s, [], L1s).
mezcla([L1|L1s], [L2|L2s], [L1|Ls]):- L1 @< L2, mezcla(L1s, [L2|L2s], Ls).
mezcla([L1|L1s], [L2|L2s], [L2|Ls]):- L1 @>= L2, mezcla(L2s, [L1|L1s], Ls).