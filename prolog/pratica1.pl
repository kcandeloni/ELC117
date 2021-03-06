% Exemplo de programa em Prolog que define
% fatos e regras sobre pessoas e localizacoes geograficas.

localizado_em(santa_maria, rs).
localizado_em(salvador, bahia).
localizado_em(rs, brasil).
localizado_em(bahia, brasil).
localizado_em(paris, franca).
localizado_em(franca, europa).

nasceu_em(andre, santa_maria).
nasceu_em(joao, salvador).
nasceu_em(joana, salvador).
nasceu_em(michel, paris).
nasceu_em(X, Y) :- localizado_em(Z, Y), nasceu_em(X, Z).

maisVelho(X,Y) :- idade(X,W), idade(Y,Z), W>Z.

mora_em(andre, paris).
mora_em(joao, salvador).
mora_em(X, Y) :- localizado_em(Z, Y), mora_em(X, Z).

soma(A,B,C) :- C is A + B.

pred(A,B,C) :- X is (A+B)^2, C is X*2+1.

idade(andre, 25).
idade(joao, 32).
idade(joana, 22).
idade(michel, 40).

anoNasc(P,A) :- idade(P,A1), A is 2016 - A1.

gaucho(X) :- nasceu_em(X, rs).
brasileiro(X) :- nasceu_em(X, brasil).
europeu(X) :- nasceu_em(X, europa).

isVowel(X) :- member(X,[a,e,i,o,u]).

%1.Qual ser� o resultado das seguintes consultas?

%?- europeu(andre).
%false
%?- gaucho(andre).
%true
%?- brasileiro(X).
%andre
%joao

%2.Expresse as seguintes afirma��es em ou ou mais fatos Prolog:

%"Joana nasceu em Salvador."
%"Joana tem 22 anos."
%"Michel nasceu em Paris e tem 40 anos."
%
%3.Expresse as seguintes perguntas em consultas Prolog:

%"Jos� nasceu no Brasil?"
%brasileiro(jose).
%"Quais s�o as pessoas nascidas na Europa?"
%europeu(X).
%"Quais s�o as pessoas com mais de 30 anos?"
%idade(X,Y),Y>30.
%"Quem s�o os brasileiros com menos de 30 anos?"
%idade(X,Y),brasileiro(X),Y<30.

%4.Crie um predicado maisVelho(A,B) que permita deduzir se a pessoa A �
% mais velha que a pessoa B. Use o predicado idade. Exemplo de uso:

%?- maisVelho(joao, andre).
%true.

% 5.Em Prolog, o operador is serve para calcular uma express�o
% aritm�tica e atribuir seu valor a uma vari�vel, como nos exemplos
% abaixo:

%soma(A,B,C) :- C is A + B.
%pred(A,B,C) :- X is (A+B)^2, C is X*2+1.
%Sabendo disso, verifique qual ser� o resultado das consultas abaixo:

%?- soma(8,5,S).
%?- pred(3,2,X).

%6.Usando o operador is, crie um predicado anoNasc(P,A) que permita
% deduzir o ano aproximado de nascimento (A) de uma pessoa (P). Use o
% predicado idade e o ano corrente como refer�ncia. Exemplo de uso:

%?- anoNasc(joao, A).
%A = 1984.

%7.Em Prolog, listas s�o tipos de dados nativos. Como em Haskell, uma
% lista se representa com colchetes, por�m em Prolog as listas podem ser
% heterog�neas. Existem tamb�m v�rios predicados pr�-definidos que
% manipulam listas. Teste as consultas abaixo e deduza o significado dos
% predicados member, length e nextto:

%?- member(a, [a,b,c]).
%?- member(x, [a,b,c]).
%?- member(A, [a,b,c]).
%?- member(a, [a,b,c,a]).
%?- length([a,b,c], L).
%?- length([], X).
%?- length(a, X).
%?- length([a,b,c], 2).
%?- nextto(1, 2, [1,2,3]).
%?- nextto(2, Y, [1,2,3]).
%?- nextto(4, X, [1,2,3]).
%?- nextto(1, 2, [1,2,3,1,2]).

%8.Usando lista em Prolog, crie um predicado isVowel(X) que verifique se
% um dado s�mbolo X � uma vogal. Exemplo de uso:

%?- isVowel(a).
%true.
%?- isVowel(b).
%false.
