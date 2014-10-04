peca([1,1,1],[1,0,1],[1,1,1]). /*Checked*/
peca([1,1,1],[1,0,0],[1,1,1]). /*Checked*/
peca([1,1,1],[1,0,1],[0,1,1]). /*Checked, modified*/
peca([1,1,1],[1,0,0],[1,0,1]). /*Checked*/
peca([1,1,1],[1,0,1],[0,0,0]). /*Checked*/
peca([1,1,1],[0,0,1],[0,1,1]). /*Checked*/
peca([1,1,1],[1,0,0],[1,1,0]). /*Checked*/
peca([1,1,1],[1,0,0],[1,0,0]). /*Checked*/
peca([1,1,1],[1,0,0],[0,0,0]). /*Checked*/
peca([1,1,1],[0,0,1],[0,0,0]). /*Checked*/
peca([1,1,1],[0,0,0],[0,0,0]). /*Checked*/
peca([1,1,0],[1,0,0],[0,0,0]). /*Checked*/
peca([0,1,1],[0,0,0],[0,0,0]). /*Checked*/
peca([1,1,0],[0,0,0],[0,0,0]). /*Checked*/
peca([1,0,0],[0,0,0],[0,0,0]). /*Checked*/
peca([0,1,0],[0,0,0],[0,0,0]). /*Checked*/

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