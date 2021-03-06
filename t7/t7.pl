/*Olimp�iada Brasileira de Inform�atica � OBI2009 � Modalidade Inicia�c�ao N�ivel 2 � Fase 1

Combate � Dengue

Uma for�ca-tarefa para combater a dengue precisa visitar
sete casas em busca de focos do mosquito. As casa
s�ao denominadas F, G, H, L, M, P e T. Deve ser feito
um plano de a�c�ao determinando a ordem em que as
casas s�ao visitadas. Para isso considera-se as seguintes
condi�c�oes:
� A casa F deve ser uma das tr�es primeiras a serem
visitadas.
� A casa H deve ser visitada imediatamente antes
da casa G.
� A casa L n�ao pode ser a primeira nem a s�etima
casa a ser visitada.
� A casa M deve ser a primeira ou a s�etima a ser
visitada.
� A casa P deve ser uma das tr�es �ultimas a serem
visitadas. */

visitaCasa(V) :- V=[_,_,_,_,_,_,_], Casas=[f,g,h,l,m,p,t], perm(Casas, V), r1(V), r2(V), r3(V), r4(V), r5(V).

perm([],[]).
perm(List, [H|Perm]) :- delete(H, List, Rest), perm(Rest, Perm).

delete(X, [X|T], T).
delete(X, [H|T], [H|NT]) :- delete(X, T, NT).

r1(V) :- V=[f,_,_,_,_,_,_].
r1(V) :- V=[_,f,_,_,_,_,_].
r1(V) :- V=[_,_,f,_,_,_,_].

r2(V) :- nextto(h,g,V).

r3(V) :- V=[_,l,_,_,_,_,_].
r3(V) :- V=[_,_,l,_,_,_,_].
r3(V) :- V=[_,_,_,l,_,_,_].
r3(V) :- V=[_,_,_,_,l,_,_].
r3(V) :- V=[_,_,_,_,_,l,_].

r4(V) :- V=[m,_,_,_,_,_,_].
r4(V) :- V=[_,_,_,_,_,_,m].

r5(V) :- V=[_,_,_,_,p,_,_].
r5(V) :- V=[_,_,_,_,_,p,_].
r5(V) :- V=[_,_,_,_,_,_,p].

/* Quest�ao 1. Qual das seguintes op�c�oes �e uma lista
completa e correta da ordem que as sete casas devem
ser visitadas?
(A) F, T, H, L, P, G e M.
(B) H, G, F, L, T, P e M. True
(C) L, T, F, H G, M e P.
(D) M, F, D, H, L, G e T.
(E) M, L, H, G, F, P e T.

visitaCasa([f,t,h,l,p,g,m]).
visitaCasa([h,g,f,l,t,p,m]).
visitaCasa([l,t,f,h,g,m,p]).
visitaCasa([m,f,d,h,l,g,t]).
visitaCasa([m,l,h,g,f,p,t]).


Quest�ao 2. Se em um trecho do percurso visitarmos
as casas T, L e F, exatamente nesta ordem, qual a
posi�c�ao que G foi visitada?

(A) Segunda.
(B) Terceira.
(C) Quarta.
(D) Quinta. True
(E) Sexta.

Obs.: As Letras (A) e (B) podem ser descartas, uma vez que a regra 1 diz
que a casa F tem de ser uma das tr�s primeiras casas a ser visitada e se
as casas T,L e F foram visitadas exatamente nessa ordem, certamente
foram a 1�, 2� e 3� casas a ser visitadas.

visitaCasa([t,l,f,g,_,_,_]).
visitaCasa([t,l,f,_,g,_,_]).
visitaCasa([t,l,f,_,_,g,_]).


Quest�ao 3. Se a casa H �e a primeira a ser visitada,
qual a quarta casa a ser visitada?
(A) F.
(B) G.
(C) L. True
(D) M.
(E) P.

visitaCasa([h,_,_,f,_,_,_]).
visitaCasa([h,_,_,g,_,_,_]).
visitaCasa([h,_,_,l,_,_,_]).
visitaCasa([h,_,_,m,_,_,_]).
visitaCasa([h,_,_,p,_,_,_]).

or

?- visitaCasa([h,_,_,X,_,_,_]).
X = l ;
X = l ;
X = t ;
X = t ; */

/* Olimp�ada Brasileira de Inform�tica � OBI2014 - Modalidade Inicia��o � N�vel 2, Fase 1

Oito alunos � Beto, Dulce, Guto, J�lia, Kelly, Neto, Silvia e Vivian decidiram tentar quebrar o recorde
da tradicional prova de revezamento e resist�ncia de nata��o que acontece todos os anos na escola.
Nessa prova, cada um dos oito competidores da equipe deve nadar mil metros, em estilo livre, na
forma de revezamento: cada nadador cai na piscina para nadar apenas uma vez, um de cada vez. O
objetivo � que todos nadem no menor tempo poss�vel. Depois de muita discuss�o, os competidores
decidiram que a ordem em que cair�o na piscina deve obedecer �s seguintes condi��es:
� Silvia n�o nada por �ltimo.
� Vivian nada ap�s J�lia e Neto nadarem.
� O primeiro a nadar � ou Beto ou Dulce.
� Guto nada antes de J�lia, com exatamente uma pessoa nadando entre eles.
� Kelly nada antes de Neto, com exatamente duas pessoas nadando entre eles. */

revesamento(R) :- R=[_,_,_,_,_,_,_,_], Nadadores=[beto,dulce,guto,julia,kelly,neto,silvia,vivian], perm(Nadadores, R), r21(R), r22(R), r23(R), r24(R), r25(R).

r21(R) :- R=[silvia,_,_,_,_,_,_,_].
r21(R) :- R=[_,silvia,_,_,_,_,_,_].
r21(R) :- R=[_,_,silvia,_,_,_,_,_].
r21(R) :- R=[_,_,_,silvia,_,_,_,_].
r21(R) :- R=[_,_,_,_,silvia,_,_,_].
r21(R) :- R=[_,_,_,_,_,silvia,_,_].
r21(R) :- R=[_,_,_,_,_,_,silvia,_].

r22(R) :- nth0(Pvivian, R, vivian), nth0(Pjulia, R, julia), nth0(Pneto, R, neto), Pvivian>Pjulia, Pvivian>Pneto.

r23([beto|_]).
r23([dulce|_]).

r24(R) :- nth0(Pguto, R, guto), nth0(Pjulia, R, julia), CPguto is Pguto + 2, Pjulia =:= CPguto.

r25(R) :- nth0(Pkelly, R, kelly), nth0(Pneto, R, neto), CPkelly is Pkelly + 3, Pneto =:= CPkelly.

vivian_beto(R) :- nth0(Pvivian, R, vivian), nth0(Pbeto, R, beto), Pvivian < Pbeto.

/* Quest�o 21. Qual das seguintes alternativas �
uma poss�vel lista completa e correta dos nadadores
do primeiro para o �ltimo?

(A) Dulce, Kelly, Silvia, Guto, Neto, Beto, J�lia,
Vivian
(B) Dulce, Silvia, Kelly, Guto, Neto, J�lia, Beto,
Vivian
(C) Beto, Kelly, Silvia, Guto, Neto, J�lia, Vivian,
Dulce %True
(D) Beto, Guto, Kelly, J�lia, Dulce, Neto, Vivian,
Silvia
(E) Beto, Silvia, Dulce, Kelly, Vivian, Guto,
Neto, J�lia

revesamento([dulce,kelly,silvia,guto,neto,beto,julia,vivian]).
revesamento([dulce,silvia,kelly,guto,neto,julia,beto,vivian]).
revesamento([beto,kelly,silvia,guto,neto,julia,vivian,dulce]). %True
revesamento([beto,guto,kelly,julia,dulce,neto,vivian,silvia]).
revesamento([beto,silvia,dulce,kelly,vivian,guto,neto,julia]).

Quest�o 22. Se Vivian nada antes de Beto, ent�o
qual dos seguintes pode ser o segundo a nadar?

(A) Silvia
(B) J�lia
(C) Neto
(D) Guto
(E) Dulce

revesamento([_,silvia,_,_,_,_,_,_]).
revesamento([_,julia,_,_,_,_,_,_]).
revesamento([_,neto,_,_,_,_,_,_]).
revesamento([_,guto,_,_,_,_,_,_]). %True
revesamento([_,dulce,_,_,_,_,_,_]).

revesamento(R),vivian_beto(R).
R = [dulce, guto, kelly, julia, silvia, neto, vivian, beto] ;
R = [dulce, kelly, silvia, guto, neto, julia, vivian, beto] ;

Quest�o 24. Guto pode nadar em qualquer das
ordens abaixo, exceto:

(A) sexto lugar	%False
(B) quinto lugar
(C) quarto lugar
(D) terceiro lugar
(E) segundo lugar

revesamento([_,_,_,_,_,guto,_,_]). %False
revesamento([_,_,_,_,guto,_,_,_]).
revesamento([_,_,_,guto,_,_,_,_]).
revesamento([_,_,guto,_,_,_,_,_]).
revesamento([_,guto,_,_,_,_,_,_]). */












