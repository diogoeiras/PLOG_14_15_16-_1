:- use_module(library(lists)).

todas_pecas(
	[
		[[1,1,1],[1,0,1],[1,1,1]],
		[[1,1,1],[1,0,0],[1,1,1]],	
		[[1,1,1],[1,0,1],[0,1,1]],
		[[1,1,1],[1,0,0],[1,0,1]],
		[[1,1,1],[1,0,1],[0,0,0]],
		[[1,1,1],[0,0,1],[0,1,1]],
		[[1,1,1],[1,0,0],[1,1,0]],
		[[1,1,1],[1,0,0],[1,0,0]],
		[[1,1,1],[1,0,0],[0,0,0]],
		[[1,1,1],[0,0,1],[0,0,0]],
		[[1,1,1],[0,0,0],[0,0,0]],
		[[1,1,0],[1,0,0],[0,0,0]],
		[[0,1,1],[0,0,0],[0,0,0]],
		[[1,1,0],[0,0,0],[0,0,0]],
		[[1,0,0],[0,0,0],[0,0,0]],
		[[0,1,0],[0,0,0],[0,0,0]]
	]).

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

start:-
	imprime_titulo,
	imprime_opcoes,
	ler_opcao(Opcao),
	write('============================'),nl,
	iniciar_jogo(Opcao).

imprime_titulo:-
	write('========= Jogo 16! ========='),
	nl.

imprime_opcoes:-
	write('|1 - Jogador contra jogador|'),nl,
	write('|2 -                   sair|'),
	nl.

tamanho_valido(Tamanho):-
	integer(Tamanho),
	Tamanho2 is Tamanho*Tamanho,
	Valor is Tamanho2 mod 16,
	Valor = 0.

ler_tamanho(Tamanho):-
	write('Escolha o tamanho do tabuleiro (NxN)'),nl,
	write('O numero de casas tem de ser multiplo de 16'),nl,
	write('> '),
	read(TamanhoAux),
	(tamanho_valido(TamanhoAux)->Tamanho is TamanhoAux;ler_tamanho(Tamanho)).

opcao_valida_menu(1).

ler_opcao(Opcao):-
	write('Escolha uma das opcoes do menu'),
	nl,
	write('> '),
	read(Opcao_aux),
	(opcao_valida_menu(Opcao_aux)->Opcao is Opcao_aux;ler_opcao(Opcao)).

/* Sair do jogo */
iniciar_jogo(2).

/* Jogador vs Jogador */
iniciar_jogo(1):-
	ler_tamanho(Tamanho),
	cria_tabuleiro(Tamanho,Tabuleiro),
	adicionar_pecas(Tamanho,Pecas),
	jogar(1,Pecas,[],Tabuleiro,1).

adicionar_pecas_aux(Pecas,Pecas,0).

adicionar_pecas_aux(Pecas,PecasAux,N):-
	todas_pecas(PecasAdicionar),
	append(PecasAux,PecasAdicionar,P),
	N2 is N-1,
	adicionar_pecas_aux(Pecas,P,N2).

adicionar_pecas(Tamanho,Pecas):-
	NPecas is Tamanho*Tamanho,
	N is NPecas // 16,
	adicionar_pecas_aux(Pecas,[],N).

jogar(N,[],_,_):-
	write('Venceu o jogador '), write(N),nl.

opcao_desistir(1).
opcao_desistir(2).

desistir(Opcao):-
	write('Pretende desistir?'),nl,
	write('1.             sim'),nl,
	write('2.             nao'),nl,
	write('> '), read(OpcaoAux),
	opcao_desistir(OpcaoAux)-> Opcao is OpcaoAux;write('Opcao invalida'),nl,desistir(opcao).

jogar(1,PecasRestantes,PecasJogadas,Tabuleiro,JogadaInicial):-
	write('Joga o jogador 1'),nl,
	imprime_tabuleiro(Tabuleiro),
	imprime_varias_pecas(1,PecasRestantes),
	desistir(Opcao),
	(Opcao = 1 -> write('Jogador 2 venceu!'),!,halt;!),
	escolhe_peca(PecasRestantes,Peca),
	append([],Peca,PecaOriginal),
	escolhe_rodar_peca(Peca,PecaFinal),
	write('Nova posicao da peca'),nl,
	imprime_peca(PecaFinal),
	escolhe_posicao_tabuleiro(Tabuleiro,X,Y,JogadaInicial),
	(jogada_valida(PecaFinal,X,Y,Tabuleiro,JogadaInicial)->insere_peca(PecaFinal,Tabuleiro,X,Y,TabuleiroFinal),imprime_tabuleiro(TabuleiroFinal),append([PecaOriginal],PecasJogadas,NovasPecasJogadas),delete(PecasRestantes,PecaOriginal,NovasPecasRestantes),jogar(2,NovasPecasRestantes,NovasPecasJogadas,TabuleiroFinal,0);write('Jogada invalida'),nl,jogar(1,PecasRestantes,PecasJogadas,Tabuleiro,JogadaInicial)).

jogar(2,PecasRestantes,PecasJogadas,Tabuleiro,JogadaInicial):-
	write('Joga o jogador 2'),nl,
	imprime_tabuleiro(Tabuleiro),
	imprime_varias_pecas(1,PecasRestantes),
	desistir(Opcao),
	(Opcao = 1 -> write('Jogador 2 venceu!'),!,halt;!),
	escolhe_peca(PecasRestantes,Peca),
	append([],Peca,PecaOriginal),
	escolhe_rodar_peca(Peca,PecaFinal),
	write('Nova posicao da peca'),nl,
	imprime_peca(PecaFinal),
	escolhe_posicao_tabuleiro(Tabuleiro,X,Y,JogadaInicial),
	(jogada_valida(PecaFinal,X,Y,Tabuleiro,JogadaInicial)->insere_peca(PecaFinal,Tabuleiro,X,Y,TabuleiroFinal),imprime_tabuleiro(TabuleiroFinal),append([PecaOriginal],PecasJogadas,NovasPecasJogadas),delete(PecasRestantes,PecaOriginal,NovasPecasRestantes),jogar(1,NovasPecasRestantes,NovasPecasJogadas,TabuleiroFinal,0);write('Jogada invalida'),nl,jogar(2,PecasRestantes,PecasJogadas,Tabuleiro,JogadaInicial)).

lado_igual([],[]).
lado_igual([H|T],[H2|T2]):-
	H = H2,
	lado_igual(T,T2).

jogada_valida(Peca,X,Y,Tabuleiro,1).

jogada_valida(Peca,X,Y,Tabuleiro,0):-
	Left is X-1,
	Right is X+1,
	Up is Y-1,
	Down is Y+1,
	proper_length(Tabuleiro,Tamanho),
	(X>1->verifica_jogada(Tabuleiro, Left, Y, 1, PecaEsquerdaLado);append([],[0,0,0],PecaEsquerdaLado)),
	(X<Tamanho->verifica_jogada(Tabuleiro, Right, Y, 3, PecaDireitaLado);append([],[0,0,0],PecaDireitaLado)),
	(Y>1->verifica_jogada(Tabuleiro, X, Up, 2, PecaCimaLado);append([],[0,0,0],PecaCimaLado)),
	(Y<Tamanho->verifica_jogada(Tabuleiro, X, Down, 4, PecaBaixoLado);append([],[0,0,0],PecaBaixoLado)),
	verifica_peca(Peca,1,PecaLadoEsquerdo),
	verifica_peca(Peca,3,PecaLadoDireito),
	verifica_peca(Peca,2,PecaLadoCima),
	verifica_peca(Peca,4,PecaLadoBaixo),
	Counter is 0,
	(lado_igual(PecaEsquerdaLado,PecaLadoEsquerdo)->Counter2 is Counter + 1;lado_igual(PecaEsquerdaLado,[0,0,0]),Counter2 is Counter),
	(lado_igual(PecaDireitaLado,PecaLadoDireito)->Counter3 is Counter2 + 1;lado_igual(PecaDireitaLado,[0,0,0]),Counter3 is Counter2),
	(lado_igual(PecaCimaLado,PecaLadoCima)->Counter4 is Counter3 + 1;lado_igual(PecaCimaLado,[0,0,0]),Counter4 is Counter3),
	(lado_igual(PecaBaixoLado,PecaLadoBaixo)->Counter5 is Counter4 + 1;lado_igual(PecaBaixoLado,[0,0,0]),Counter5 is Counter4),
	Counter5 > 0.

opcao_valida_posicao(OpcaoX,OpcaoY,Tabuleiro,1):-
	proper_length(Tabuleiro,Tamanho),
	Meio is Tamanho//2,
	integer(OpcaoX),
	integer(OpcaoY),
	OpcaoX > Meio - 1,
	OpcaoY > Meio - 1,
	OpcaoX =< Meio + 1,
	OpcaoY =< Meio + 1.

opcao_valida_posicao(OpcaoX,OpcaoY,Tabuleiro,0):-
	proper_length(Tabuleiro,Tamanho),
	integer(OpcaoX),
	integer(OpcaoY),
	OpcaoX > 0,
	OpcaoY > 0,
	OpcaoX =< Tamanho,
	OpcaoY =< Tamanho,
	Valor is OpcaoY * 4,
	Valor2 is OpcaoX + Valor,
	nth1(OpcaoY, Tabuleiro, L),
	nth1(OpcaoX, L, [[0,0,0],[0,0,0],[0,0,0]]).

escolhe_posicao_tabuleiro(Tabuleiro,X,Y,JogadaInicial):-
	write('Escolha a posicao onde quer inserir a peca'),nl,
	write('Posicao X: > '), read(OpcaoX),
	write('Posicao Y: > '), read(OpcaoY),
	opcao_valida_posicao(OpcaoX,OpcaoY,Tabuleiro,JogadaInicial)->X is OpcaoX,Y is OpcaoY;write('Opcao invalida'),nl,escolhe_posicao_tabuleiro(Tabuleiro,X,Y,JogadaInicial).

opcao_valida_rodar(1).
opcao_valida_rodar(2).
opcao_valida_rodar(3).
opcao_valida_rodar(4).

escolhe_rodar_peca(Peca,PecaFinal):-
	write('Pretende rodar a peca?'),nl,
	write('1 -              Nao'),nl,
	write('2 -  90graus direita'),nl,
	write('3 - 180graus direita'),nl,
	write('4 - 270graus direita'),nl,
	write('> '),
	read(Opcao),
	(opcao_valida_rodar(Opcao)->escolhe_rodar_peca_aux(Peca,PecaFinal,1,Opcao);write('Opcao invalida'),nl,escolhe_rodar_peca(Peca,PecaFinal)).

escolhe_rodar_peca_aux(Peca,Peca,N,N).

escolhe_rodar_peca_aux(Peca,PecaFinal,Atual,Fim):-
	rodar_peca_direita(Peca,P),
	Next is Atual + 1,
	escolhe_rodar_peca_aux(P,PecaFinal,Next,Fim).

escolhe_peca(PecasRestantes,Peca):-
	write('Escolha a peca'),nl,
	write('> '),
	read(Opcao),
	proper_length(PecasRestantes,Tamanho),
	opcao_valida_escolha_peca(Opcao, Tamanho) -> nth1(Opcao,PecasRestantes,Peca);write('Opcao invalida'),nl,escolhe_peca(PecasRestantes,Peca).

opcao_valida_escolha_peca(Opcao, N):-
	integer(Opcao),
	Opcao > 0,
	Opcao =< N.

imprime_varias_pecas(_,[]).

imprime_varias_pecas(N,[H|T]):-
	write(N),
	write(':-'),nl,imprime_peca(H),nl,
	Next is N + 1,
	imprime_varias_pecas(Next,T).

imprime_peca(Peca):-
	write('|'),imprime_parte_peca(Peca,0,0),nl,
	write('|'),imprime_parte_peca(Peca,1,0),nl,
	write('|'),imprime_parte_peca(Peca,2,0),nl.

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
			append(Final,[H],Final_aux),
			Atual1 is Atual + 1,
			insere_peca_aux(Peca, T, X, Y, Final_aux, N, Atual1, Tabuleiro).

insere_peca_linha(Peca, Linha, X, LinhaR):-
		proper_length(Linha,N),
		insere_peca_linha_aux(Peca, Linha, X ,[], N, 0, LinhaR).

insere_peca_linha_aux(_, _, _, LinhaTemp, N, N, LinhaTemp).

insere_peca_linha_aux(Peca, [_|T], X ,LinhaTemp, N, X, LinhaR):-
			append(LinhaTemp, [Peca], LinhaTemp_aux),
			Atual1 is X + 1,
			insere_peca_linha_aux(Peca, T, X, LinhaTemp_aux, N, Atual1, LinhaR).

insere_peca_linha_aux(Peca, [H|T], X ,LinhaTemp, N, Atual, LinhaR):-
			append(LinhaTemp,[H],LinhaTemp_aux),
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