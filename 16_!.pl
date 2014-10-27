peca([[1,1,1],[1,0,1],[1,1,1]]). 
peca([[1,1,1],[1,0,0],[1,1,1]]). 
peca([[1,1,1],[1,0,1],[0,1,1]]). 
peca([[1,1,1],[1,0,0],[1,0,1]]). 
peca([[1,1,1],[1,0,1],[0,0,0]]). 
peca([[1,1,1],[0,0,1],[0,1,1]]). 
peca([[1,1,1],[1,0,0],[1,1,0]]). 
peca([[1,1,1],[1,0,0],[1,0,0]]). 
peca([[1,1,1],[1,0,0],[0,0,0]]). 
peca([[1,1,1],[0,0,1],[0,0,0]]). 
peca([[1,1,1],[0,0,0],[0,0,0]]). 
peca([[1,1,0],[1,0,0],[0,0,0]]). 
peca([[0,1,1],[0,0,0],[0,0,0]]). 
peca([[1,1,0],[0,0,0],[0,0,0]]). 
peca([[1,0,0],[0,0,0],[0,0,0]]). 
peca([[0,1,0],[0,0,0],[0,0,0]]). 


/*
cria_tabuleiro(N, L):-                               
			N >= 4,
			N1 is N mod 4,
			N1 == 0,
			cria_tabuleiro_N(N, [], L).


cria_tabuleiro_N(0, [H|T], [H|T]).

cria_tabuleiro_N(N, [H|T], L):-
			cria_linha(N, [], H),
			N1 is N-1,
			cria_tabuleiro_N(N1, T, L).


cria_linha(0, Tmp, Tmp).

cria_linha(N, Tmp, H):-
		append([0,0,0], Tmp),
		N1 is N-1,
		cria_linha(N1, Tmp, H).

		*/
		/*Mudar este estilo para tamanho de peça* -- append([0,0,0], Tmp),*/

		/*joga_peça(peça, tabuleiro, posx, posy, Tabuleiro final)*/

joga_peca(Peca, Tabuleiro, X ,Y, Final):-
		replace(Peca, Tabuleiro, X, Y, Final).


replace(Peca, Tabuleiro, X, Y, Final):-
		insert(Peca, Tabuleiro, X, Y, Final).

insert(Peca, Tabuleiro, X, Y, Final):-
	got_to_y(Peca, Tabuleiro, X, Y, Final).

go_to_x([], Final, _, Final).

go_to_x([Hp|Tp], [H|T], X, Final):-
		X == 0,
		/*AQUI TENHO QUE IR A CADA ELEMENTO DE HP E METER ESSE ELEMENTO EM H -- NAO SEI COMO :S*/
		go_to_x(Tp, T, X, Final).

go_to_x(Peca, [_|T], X, Final):-
		X > 0,
		X1 is X-1,
		go_to_x(Peca, T, X1, Final).

got_to_y(Peca, [H|_], X, Y, Final):-
		Y == 0,
		go_to_x(Peca, H, X, Final).


got_to_y(Peca, [H|T], X, Y, Final):-
		Y > 0,
		Y1 is Y-1,
		got_to_y(Peca, T, X, Y1, Final).
		



		