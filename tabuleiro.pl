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


tabuleiro([
		   [[],[],[],[]],
		   [[],[],[],[]],
		   [[],[],[],[]],
		   [[],[],[],[]]
		  ]).

tabuleiro_exemplo([
		   [[[0,1,0],[0,0,0],[0,0,0]],[[1,0,0],[0,0,0],[0,0,0]],[[1,1,0],[0,0,0],[0,0,0]],[[0,1,1],[0,0,0],[0,0,0]]],
		   [[[0,1,0],[0,0,0],[0,0,0]],[[1,0,0],[0,0,0],[0,0,0]],[[1,1,0],[0,0,0],[0,0,0]],[[0,1,1],[0,0,0],[0,0,0]]],
		   [[[0,1,0],[0,0,0],[0,0,0]],[[1,0,0],[0,0,0],[0,0,0]],[[1,1,0],[0,0,0],[0,0,0]],[[0,1,1],[0,0,0],[0,0,0]]],
		   [[[0,1,0],[0,0,0],[0,0,0]],[[1,0,0],[0,0,0],[0,0,0]],[[1,1,0],[0,0,0],[0,0,0]],[[0,1,1],[0,0,0],[0,0,0]]]
		  ]).

imprime_linhas(_,[]):-
		write('|=======================================|'),
		nl.

imprime_linhas(N,[LinhaTabuleiro|RestoTabuleiro]):-
		imprime_linha(0,LinhaTabuleiro,LinhaTabuleiro),
		N1 is N + 1,
		imprime_linhas(N1,RestoTabuleiro).

imprime_parte_peca([ParteImprimir|_],N,N):-
		write(ParteImprimir).

imprime_parte_peca([_|Resto],N,Pos):-
		Pos < 2,
		Pos1 is Pos + 1,
		imprime_parte_peca(Resto,N,Pos1).

imprime_linha(3,_,_):-
		nl.	

imprime_linha(N,[],Original):-
		write('|'),
		nl,
		N1 is N+1,
		imprime_linha(N1,Original,Original).

imprime_linha(N,[Peca|RestoLinha],Original):-
		integer(N),
		N >= 0,
		write('| '),
		imprime_parte_peca(Peca,N,0),
		write(' '),
		imprime_linha(N,RestoLinha,Original).

imprime_tabuleiro(Tabuleiro):-
		write('|=======================================|'),
		nl,nl,
		imprime_linhas(0,Tabuleiro).
