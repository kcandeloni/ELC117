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
visitadas. 

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

visitaCasa(t,l,f,g,_,_,_).
visitaCasa(t,l,f,_,g,_,_).
visitaCasa(t,l,f,_,_,g,_).


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









