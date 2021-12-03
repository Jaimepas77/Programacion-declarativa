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
%mezcla([], [], []).%Innecesario
mezcla([], L2s, L2s).
mezcla(L1s, [], L1s).
mezcla([L1|L1s], [L2|L2s], [L1|Ls]):- L1 @< L2, mezcla(L1s, [L2|L2s], Ls).
mezcla([L1|L1s], [L2|L2s], [L2|Ls]):- L1 @>= L2, mezcla(L2s, [L1|L1s], Ls).

%3
simetricas(_, []).
simetricas([Xs|Xss], [Ys|Yss]):- Xs = Ys, invertir(Xs, [], Xs), simetricas(Xss, Yss).
simetricas([_|Xss], [Ys|Yss]):- simetricas(Xss, [Ys|Yss]).

%Se crea invertir al igual que en las transparencias. Se usa tal que la inversa de una lista tiene que ser ella misma (definición de simétrica)
invertir([], Ys, Zs):- Ys = Zs.
invertir([X|Xs], Ys, Zs):- invertir(Xs, [X|Ys], Zs).

%4
numnodos(0, void).
numnodos(X, arbol(_, Y, Z)):- numnodos(N1, Y), numnodos(N2, Z), X is N1+N2+1.