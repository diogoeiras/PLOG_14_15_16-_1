:- use_module(library(lists)).

peca([1,1,1],[1,0,1],[1,1,1]). 
peca([1,1,1],[1,0,0],[1,1,1]). 
peca([1,1,1],[1,0,1],[0,1,1]). 
peca([1,1,1],[1,0,0],[1,0,1]). 
peca([1,1,1],[1,0,1],[0,0,0]). 
peca([1,1,1],[0,0,1],[0,1,1]). 
peca([1,1,1],[1,0,0],[1,1,0]). 
peca([1,1,1],[1,0,0],[1,0,0]). 
peca([1,1,1],[1,0,0],[0,0,0]). 
peca([1,1,1],[0,0,1],[0,0,0]). 
peca([1,1,1],[0,0,0],[0,0,0]). 
peca([1,1,0],[1,0,0],[0,0,0]). 
peca([0,1,1],[0,0,0],[0,0,0]). 
peca([1,1,0],[0,0,0],[0,0,0]). 
peca([1,0,0],[0,0,0],[0,0,0]). 
peca([0,1,0],[0,0,0],[0,0,0]). 


/*

	Mudar -- Tabuleiro pode ser 'infinito', sendo que deve aumentar em proporções
			quadraticas, permitindo um aumento de peças isto implica uma readaptação 
			ja jogabilidade pois vai ter de se incrementar a quantidade de peças 
			(ex: se 2*tabuleiro -> 2*(nº total de peças))
	Testar -- Imprimir mais tabuleiros e imprimir tabuleiros maiores

*/


imprime_linhas(_,[]).

imprime_linhas(N,[LinhaTabuleiro|RestoTabuleiro]):-
		imprime_linha(0,LinhaTabuleiro,LinhaTabuleiro),
		N1 is N + 1,
		imprime_linhas(N1,RestoTabuleiro).

imprime_parte_a_imprimir([]):-
		write('|').


imprime_parte_a_imprimir([0]):-
		write(' '),
		imprime_parte_a_imprimir([]).

imprime_parte_a_imprimir([1]):-
		write('X'),
		imprime_parte_a_imprimir([]).

imprime_parte_a_imprimir([0|Resto]):-
		write('  '),
		imprime_parte_a_imprimir(Resto).

imprime_parte_a_imprimir([1|Resto]):-
		write('X '),
		imprime_parte_a_imprimir(Resto).


imprime_parte_peca([ParteImprimir|_],N,N):-
		imprime_parte_a_imprimir(ParteImprimir).

imprime_parte_peca([_|Resto],N,Pos):-
		Pos < 2,
		Pos1 is Pos + 1,
		imprime_parte_peca(Resto,N,Pos1).

imprime_linha(3,_,_):-
		nl.	

imprime_linha(N,[],Original):-
		nl,
		N1 is N+1,
		imprime_linha(N1,Original,Original).

imprime_linha(N,[Peca|RestoLinha],Original):-
		integer(N),
		N >= 0,
		write('|'),
		imprime_parte_peca(Peca,N,0),
		write(' '),
		imprime_linha(N,RestoLinha,Original).

imprime_tabuleiro(Tabuleiro):-
		nl,
		imprime_linhas(0,Tabuleiro).

imprime_tabuleiro_exemplo:-
		nl,
		imprime_linhas(0,[
		   [[[0,1,0],[0,0,0],[0,0,0]],[[1,0,0],[0,0,0],[0,0,0]],[[1,1,0],[0,0,0],[0,0,0]],[[0,1,1],[0,0,0],[0,0,0]]],
		   [[[1,1,1],[0,0,0],[1,1,1]],[[1,0,0],[0,1,1],[0,0,0]],[[1,1,0],[0,1,1],[0,0,0]],[[0,1,1],[0,1,1],[0,0,0]]],
		   [[[0,1,0],[0,1,1],[0,0,0]],[[1,0,0],[0,0,0],[0,1,1]],[[0,1,1],[0,0,0],[0,1,1]],[[0,1,1],[0,0,0],[0,1,1]]],
		   [[[0,1,0],[0,0,0],[0,0,0]],[[1,0,0],[0,0,0],[0,0,0]],[[1,1,0],[0,0,0],[0,0,0]],[[0,1,1],[0,1,1],[0,1,1]]]
		  ]).

adicionar_parte_rodada([Inserir|Resto],Linha1,Linha2,Linha3,0,A,B,C):-
		append([Inserir],Linha1,A),
		adicionar_parte_rodada(Resto,Linha1,Linha2,Linha3,1,A,B,C).

adicionar_parte_rodada([Inserir|Resto],Linha1,Linha2,Linha3,1,A,B,C):-
		append([Inserir],Linha2,B),
		adicionar_parte_rodada(Resto,Linha1,Linha2,Linha3,2,A,B,C).

adicionar_parte_rodada([Inserir|Resto],Linha1,Linha2,Linha3,2,A,B,C):-
		append([Inserir],Linha3,C),
		adicionar_parte_rodada(Resto,Linha1,Linha2,Linha3,3,A,B,C).

adicionar_parte_rodada(_,_,_,_,3,_,_,_).

rodar_peca_direita(Peca,PecaFinal):-
		rodar_peca_direita_aux(Peca,0,[],[],[],PecaFinal).

rodar_peca_direita_aux([],0,[],[],[],_).

rodar_peca_direita_aux(_,3,A,B,C,PecaFinal):-
		append([A],[B],L),
		append(L,[C],PecaFinal),
		rodar_peca_direita_aux([],0,[],[],[],PecaFinal).

rodar_peca_direita_aux([Parte|Resto],Linha,Linha1,Linha2,Linha3,PecaFinal):-
		integer(Linha),
		Linha >= 0,
		Linha < 3,
		adicionar_parte_rodada(Parte,Linha1,Linha2,Linha3,0,A,B,C),
		LinhaSegue is Linha + 1,
		rodar_peca_direita_aux(Resto,LinhaSegue,A,B,C,PecaFinal).

		
/*
rodar_peca_direita_aux_linha([Parte|Resto],N,TamanhoPeca,PecaFinal,PecaFinalAux):-
		integer(N),
		N < TamanhoPeca,
		nth0(N,PecaFinal,LinhaFinal),
		append([Parte],LinhaFinal,NovaLista),
		N1 is N + 1,
		rodar_peca_direita_aux_linha(Resto,N1,TamanhoPeca,NovaLista).


rodar_peca_direita_aux([Linha|RestoLinha],N,Tamanho,PecaFinal):-
		integer(N),
		N < Tamanho,
		proper_length(Linha,TamanhoPeca),
		rodar_peca_direita_aux_linha(Linha,0,TamanhoPeca,PecaFinal,[]),



rodar_peca_direita(Peca,PecaFinal):-
		proper_length(Peca,Tamanho),
		rodar_peca_direita_aux(Peca,0,Tamanho,PecaFinal).
*/