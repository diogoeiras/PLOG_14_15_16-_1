:- use_module(library(lists)).
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



/*Funciona mas há aquele stresse dos parenteses*/
joga_peca(Peca, Tabuleiro, X ,Y, Final):-
			insere_peca(Peca, Tabuleiro, X ,Y, Final).

insere_peca(Peca, Tabuleiro, X ,Y, Final):-
		proper_length(Tabuleiro,N),
		X1 is X-1,
		Y1 is Y-1,
		insere_peca_aux(Peca, Tabuleiro, X1, Y1, [], N, 0, Final).

insere_peca_aux(_, _, _, _, Final, N, N, Final).

insere_peca_aux(Peca, [H|T], X, Y, Final, N, Y, Tabuleiro):-
		insere_peca_linha(Peca, H, X, LinhaR),
		append(Final, [LinhaR], Final_aux),/*meti aqui o []*/
		/*Fazer o apend ja para o final*/
		Atual1 is Y + 1,
		insere_peca_aux(Peca, T, X, Y, Final_aux, N, Atual1, Tabuleiro).

insere_peca_aux(Peca, [H|T], X, Y, Final, N, Atual, Tabuleiro):-
			append(Final,H,Final_aux),
			Atual1 is Atual + 1,
			insere_peca_aux(Peca, T, X, Y, Final_aux, N, Atual1, Tabuleiro).

insere_peca_linha(Peca, Linha, X, LinhaR):-
		proper_length(Linha,N),
		insere_peca_linha_aux(Peca, Linha, X ,[], N, 0, LinhaR).

insere_peca_linha_aux(_, _, _, LinhaTemp, N, N, LinhaTemp).

insere_peca_linha_aux(Peca, [_|T], X ,LinhaTemp, N, X, LinhaR):-
			append(LinhaTemp, Peca, LinhaTemp_aux),
			Atual1 is X + 1,
			insere_peca_linha_aux(Peca, T, X, LinhaTemp_aux, N, Atual1, LinhaR).

insere_peca_linha_aux(Peca, [H|T], X ,LinhaTemp, N, Atual, LinhaR):-
			append(LinhaTemp,H,LinhaTemp_aux),
			Atual1 is Atual+1,
			insere_peca_linha_aux(Peca, T, X, LinhaTemp_aux, N, Atual1, LinhaR).

/*Devolve o lado da peça*/
		/*if 1- lado esquerdo
		  if 2- lado de cima
		  if 3- lado direito
		  if 4- lado de baixo*/

verifica_peca(Peca, Pos, SideLine):-
	proper_length(Peca, N),
	verifica_peca_aux(Pos, Peca, N, [], SideLine).

verifica_peca_aux(4, Peca, N, _, SideLine):-
		nth1(N, Peca, SideLine).

verifica_peca_aux(2, Peca, _, _, SideLine):-
		nth1(1, Peca, SideLine).

verifica_peca_aux(1, _, 0, SideLine, SideLine).

verifica_peca_aux(1, [H|T], N, SideLineTemp, SideLine):-
		N>0,
		nth1(1, H, Hl),
		append(SideLineTemp, [Hl], SideLineA),
		N1 is N-1,
		verifica_peca_aux(1, T, N1, SideLineA, SideLine).

verifica_peca_aux(3, _, 0, SideLine, SideLine).

verifica_peca_aux(3, [H|T], N, SideLineTemp, SideLine):-
		N>0,
		proper_length(H, Tamanho),
		nth1(Tamanho, H, Hl),
		append(SideLineTemp, [Hl], SideLineA),
		N1 is N-1,
		verifica_peca_aux(3, T, N1, SideLineA, SideLine).


/*Onde é que a peça esta relativa à peça que vai ser jogada!
Ou seja se a peça esta do lado esquero da que vai ser jogada entao devolve o interior direito dessa peça!*/
		/*if 1- esquerda
		  if 2- cima
		  if 3- direita
		  if 4- baixo*/

verifica_jogada(Tabuleiro, X, Y, Pos, SideLine):-
		nth1(Y, Tabuleiro, LinhaTemp),
		nth1(X, LinhaTemp, PecaTemp),
		proper_length(PecaTemp, N),
		verifica_jogada_aux(Pos, PecaTemp, N,[], SideLine).



verifica_jogada_aux(2, PecaTemp, N, _, SideLineF):-
	nth1(N, PecaTemp, SideLineF).

verifica_jogada_aux(4, PecaTemp, _, _, SideLineF):-
	nth1(1, PecaTemp, SideLineF).

verifica_jogada_aux(3, _, 0, SideLineF, SideLineF).

verifica_jogada_aux(3, [H|T], N, SideLine, SideLineF):-
	N>0,
	nth1(1, H, Hl),
	append(SideLine, [Hl], SideLineA),
	N1 is N-1,
	verifica_jogada_aux(3, T, N1, SideLineA, SideLineF).

verifica_jogada_aux(1, _, 0, SideLineF, SideLineF).

verifica_jogada_aux(1, [H|T], N, SideLine, SideLineF):-
	N>0,
	proper_length(H, Tamanho),
	nth1(Tamanho, H, Hl),
	append(SideLine, [Hl], SideLineA),
	N1 is N-1,
	verifica_jogada_aux(1, T, N1, SideLineA, SideLineF).


/*Acho que ja esta a funcao de criar tabuleiro! Testei ...*/


cria_tabuleiro(N, Tabuleiro):-                               
			N >= 4,
			N1 is N mod 4,
			N1 == 0,
			Ciclo is N,
			cria_tabuleiro_aux(Ciclo, N, [], Tabuleiro).

cria_tabuleiro_aux(0, _, Tabuleiro, Tabuleiro).
cria_tabuleiro_aux(Ciclo, N, TabuleiroTemp, Tabuleiro):-
			cria_linha(N, [], Linha),
			append(TabuleiroTemp, [Linha], TabuleiroTempA),
			Ciclo1 is Ciclo - 1,
			cria_tabuleiro_aux(Ciclo1, N, TabuleiroTempA, Tabuleiro).

cria_linha(0, Linha, Linha).

cria_linha(N, LinhaTemp, Linha):-
			append(LinhaTemp, [[[0,0,0],[0,0,0],[0,0,0]]], LinhaTempA),
			N1 is N-1,
			cria_linha(N1, LinhaTempA, Linha).



/*


cria_tabuleiro_aux(N, TabuleiroTemp, Tabuleiro):-
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