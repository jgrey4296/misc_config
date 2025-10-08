%% simple.lp -*- mode: pasp -*-
#program preference(subset).


% a comment

% commands
#show.
#show d.
#show d/2.
#heuristic a. [ 1, sign]
#include "blah.lp".
#include <blah>.
#external q(X) : p(X).
#const count = 5.

% Facts
course( 1,1,5 ).
b.
something(else, blah).

% nested facts
holds(on(0,1),2).

% Selection
{a;b;c;d}.
2{a;b;c;d}4.
blah( a; b; c).

% ranges
time(1..5).

% Basic rules
a :- b.

c :- b,
     d.

1{ d(X) }1 :- a(X).

% Variables
d(X, Y) :- X=b, Y=d.

% anon variables
d(X) :- d(X, _).

% strings
e("a string") :- b.

% operators
plus    ( L +  R ) :- left(L), right(R).
minus   ( L -  R ) :- left(L), right(R).
uminus  (   -  R ) :-          right(R).
times   ( L *  R ) :- left(L), right(R).
divide  ( L /  R ) :- left(L), right(R).
modulo  ( L \  R ) :- left(L), right(R).
absolute(|  -  R|) :-          right(R).
power   ( L ** R ) :- left(L), right(R).
bitand  ( L &  R ) :- left(L), right(R).
bitor   ( L ?  R ) :- left(L), right(R).
bitxor  ( L ^  R ) :- left(L), right(R).
bitneg  (   ~  R ) :-          right(R).
eq (X,Y) :- X = Y, sym(X), sym(Y).
neq(X,Y) :- X != Y, sym(X), sym(Y).
lt (X,Y) :- X <  Y, sym(X), sym(Y).
leq(X,Y) :- X <= Y, sym(X), sym(Y).
gt (X,Y) :- X >  Y, sym(X), sym(Y).
geq(X,Y) :- X >= Y, sym(X), sym(Y).

%% negation
a :- not b, -b.

% internal rules/facts
_error.
_error :- blah; _error.

% Integrity constraint
:- aweg.

% Complex rules
better(S) :- preference(S,subset),
             not holds(A),      holds'(A), preference(S,_,_,for(A),_),
             not holds(B) : not holds'(B), preference(S,_,_,for(B),_).

better(P) :- preference(P,less(cardinality)),
            1 #sum { -1,X : holds(X),  preference(P,_,_,for(X),_);
            1,X : holds'(X), preference(P,_,_,for(X),_)}.


% @cat use
something(M) :- M = @cat("pareto: In preference element ",N," of preference statement ",
                P,", only naming atoms are allowed."),
                preference(P,pareto),
                preference(P,(N,_),_,for(X),_).
