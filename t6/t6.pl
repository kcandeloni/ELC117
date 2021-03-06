repete(0, _, []).
repete(N, C, L) :- N > 0, L = [C | T], N1 is N - 1, repete(N1, C, T).

%1.
zeroInit([H|_]) :- H =:= 0.
%2.
has5(L) :- length(L,N), N =:= 5.
%3.
hasN(N,L) :- length(L,M), N =:= M.
%4.
potN0(0,[1]).
potN0(N,L) :- N > 0, C is 2 ^ N, L = [C | T], N1 is N - 1, potN0(N1,T).
%5.
zipmult([],[],[]).
zipmult([H1|T1],[H2|T2],L3) :- L1 is H1*H2, L3 = [L1|T3], zipmult(T1,T2,T3).
%6.
potencias1(0,_,[]).
potencias1(N,P,L) :- N > 0, N1 is N -1, X is 2^P, L = [X | T], P1 is P +1, potencias1(N1, P1, T).
potencias(N,L) :- potencias1(N,0,L).
%7.
positivos([],[]).
positivos([H|T],L2) :- H > 0, L2 = [H | T2], positivos( T, T2 ).
positivos([_|T],L2) :- positivos(T,L2).
%8.
mesmaPosicao(H,[H|_],[H|_]).
mesmaPosicao(A,[_|T],[_|T1]) :- mesmaPosicao(A,T,T1).
%9.
insereInicio(H, L, [H|L]):- !.
remove(0,[_|T],T):-!.
remove(I,[H|T],R) :- X is I -1, remove(X,T,Y), insereInicio(H,Y,R).%ref -> https://pt.wikibooks.org/wiki/Prolog/Exemplos
comissao(NP,LP,L) :- NP >= 0, remove(NP,LP,L); NP1 is NP - 1, comissao(NP1,LP,L).
%10.
azulejos(0,0).
azulejos(NA, T) :- NA > 0, N1 is floor(sqrt(NA)) ^ 2, NA1 is NA - N1, azulejos(NA1, R), T is R+1.
/*Qual ser� o resultado das seguintes consultas?

?- repete(1,a,L).
?- repete(2,b,L).
?- repete(-2,b,L).
?- repete(2,a,[a,b,c]).
?- repete(3,a,[a|T]).*/

/*1.Defina um predicado zeroInit(L) que � verdadeiro se L for uma lista que inicia com o n�mero 0 (zero). Exemplo de uso:

?- zeroInit([9,6,7]).
false.
?- zeroInit([0,6,7]).
true.

2.Defina um predicado has5(L) que � verdadeiro se L for uma lista de 5
elementos. Resolva este exerc�cio sem usar um predicado auxiliar.

3.Defina um predicado hasN(L,N) que � verdadeiro se L for uma lista de N
elementos.

4.Defina um predicado potN0(N,L), de forma que L seja uma lista de
pot�ncias de 2, com expoentes de N a 0. Exemplo de uso:

?- potN0(7,L).
L = [128, 64, 32, 16, 8, 4, 2, 1]

5.Defina um predicado zipmult(L1,L2,L3), de forma que cada elemento da
lista L3 seja o produto dos elementos de L1 e L2 na mesma posi��o do
elemento de L3. Exemplo:

?� zipmult([1,2,3],[2,2,2],L).
L = [2, 4, 6].

6.Defina um predicado potencias(N,L), de forma que L seja uma lista com
as N primeiras pot�ncias de 2, sendo a primeira 2^0 e assim por diante,
conforme o exemplo abaixo:

?� potencias(5,L).
L = [1, 2, 4, 8, 16]
?� potencias(0,L).
L = []
Dica: defina um predicado auxiliar.

7.Defina um predicado positivos(L1,L2), de forma que L2 seja uma lista
s� com os elementos positivos de L1, conforme o exemplo abaixo:

?� positivos([�-1,0,1,-�2,9],L).
L = [1, 9]

8.Considere que L1 e L2 sejam permuta��es de uma lista de elementos
distintos, sem repeti��es. Sabendo disso, defina um predicado
mesmaPosicao(A,L1,L2) para verificar se um elemento A est� na mesma
posi��o nas listas L1 e L2. Exemplo de uso:

?� mesmaPosicao(c,[a,b,c,d,e],[e,d,c,b,a]).
true
?� mesmaPosicao(b,[a,b,c,d,e],[e,d,c,b,a]).
false

9.Dada uma lista de N alunos, deseja-se escolher NP alunos (NP < N) para
formar uma comiss�o. Para isso, defina um predicado comissao(NP,LP,C),
que permita gerar as poss�veis combina��es C com NP elementos da lista
LP. Exemplo:

?� comissao(3,[maria,jose,joao,mario],C).
C = [maria, jose, joao] ;
C = [maria, jose, mario] ;
C = [maria, joao, mario] ;
C = [jose, joao, mario]
?� comissao(0,[maria,jose,joao,mario],C).
C = []

10.(Adaptado de OBI2006-F1N1) Tem-se N azulejos 10cm x 10cm e, com eles,
deve-se montar um conjunto de quadrados de modo a utilizar todos os
azulejos dados, sem sobrep�-los. Inicialmente, deve-se montar o maior
quadrado poss�vel; ent�o, com os azulejos que sobraram, deve-se montar o
maior quadrado poss�vel, e assim sucessivamente. Por exemplo, se forem
dados 31 azulejos, o conjunto montado ter� 4 quadrados. Para resolver
este problema, voc� dever� definir um predicado azulejos(NA, NQ), de
forma que NQ seja o n�mero de quadrados que se deve montar com NA
azulejos. Dica: use os predicados sqrt e floor, pr�-definidos em
Prolog.*/
