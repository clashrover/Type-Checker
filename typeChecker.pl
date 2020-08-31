bool(true).
bool(false).

hasType(G,N,int) :- integer(N).

hasType(G,B,bool) :- bool(B).

hasType(G,var(X),T) :-  lookup(G,var(X),T).

hasType(G,proj1(A),T) :- hasType(G,A,[T,Tm]).
hasType(G,proj2(A),T) :- hasType(G,A,[Tm,T]).

hasType(G,sum(M,N), int) :- hasType(G,M,int), hasType(G,N,int).
hasType(G,subt(M,N),int) :- hasType(G,M,int), hasType(G,N,int).
hasType(G,mult(M,N),int) :- hasType(G,M,int), hasType(G,N,int).

hasType(G,and(M,N),bool) :- hasType(G,M,bool), hasType(G,N,bool).
hasType(G,or(M,N),bool) :- hasType(G,M,bool), hasType(G,N,bool).
hasType(G,xor(M,N),bool) :- hasType(G,M,bool), hasType(G,N,bool).


hasType(G,compare(M,N),bool) :- hasType(G,M,T), hasType(G,N,T).
hasType(G,equal(M,N),bool) :- hasType(G,M,T), hasType(G,N,T).

hasType(G,if(M,N,P),T) :- hasType(G,M,bool), hasType(G,N,T), hasType(G,P,T).

hasType(G,let(D,E),T) :- typeElaborates(G,D,G1), cascade(G,G1,G2), hasType(G2,E,T).

hasType(G,tuple([]),[]).
hasType(G,tuple([Head|Tail]),[Type|TypeTail]) :- hasType(G,Head,Type), hasType(G,tuple(Tail),TypeTail).  


hasType(G,lambda(X, E), arrow(Tm,T)) :- hasType(G,X,Tm), hasType(G,E,T). 

hasType(G,funcall(lambda(X,E), E2),T) :- hasType(G,E2,Tm),  cascade(G,[(X,Tm)],G1),  hasType(G1,lambda(X,E), arrow(Tm,T)).



typeElaborates(G,def(X,E),G1) :- hasType(G,E,T), cascade([], [(X,T)],G1).

typeElaborates(G,seq(D1,D2),G1) :- typeElaborates(G,D1,G11), cascade(G, G11, G12) ,typeElaborates(G12,D2,G13), cascade(G11, G13, G1).

typeElaborates(G,parallel(D1,D2),G1) :- typeElaborates(G,D1,G11), typeElaborates(G,D2,G12), cascade(G11, G12, G1).

typeElaborates(G,local(D1,D2),G1) :- typeElaborates(G,D1,G11), cascade(G,G11,G12), typeElaborates(G12,D2,G1).

cascade(G1,[],G1).
cascade(G1,[Head|Tail],[Head|Rest]) :- cascade(G1, Tail, Rest).

lookup([(X,T)|Tail],X,T) :- !.
lookup([(Y,Type)|Tail],X,T) :- lookup(Tail,X,T).

hasType(G, eq(A,B), T) :- hasType(G,A,T), hasType(G,B,T).

initGamma([]).
