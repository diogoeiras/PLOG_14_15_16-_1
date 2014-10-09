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


/*Mudar -- Tabuleiro pode ser 'infinito', sendo que deve aumentar em proporções
			quadraticas, permitindo um aumento de peças isto implica uma readaptação 
			ja jogabilidade pois vai ter de se incrementar a quantidade de peças 
			(ex: se 2*tabuleiro -> 2*(nº total de peças))*/


tabuleiro([[[0,0,0],[0,0,0],[0,0,0],[0,0,0]],
		   [[0,0,0],[0,0,0],[0,0,0],[0,0,0]],
		   [[0,0,0],[0,0,0],[0,0,0],[0,0,0]],
		   [[0,0,0],[0,0,0],[0,0,0],[0,0,0]]]).

/*Can't print!               Print test general purpose!*/
print(0, _) :- !.
print(_, []).
print(N, [H|T]) :- 
				write(H),
				write(" , "),
				N1 is N - 1,
				print(N1 , T).
/*Functions not complete! Do no work!*/

/*
lists([], []).
lists([[Head|_]|Lists], [Head|L]):-  lists(Lists, L).
lists([[_,Head|Tail]|Lists], L):-  lists([[Head|Tail]|Lists], L).
*/