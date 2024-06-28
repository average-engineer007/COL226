var_type(X,intT,[]):-false.
var_type(X,intT,[(X,intT)|_]) :- true.
var_type(X,intT,[_|R]) :- var_type(X,intT,R).

var_type(X,boolT,[]):-false.
var_type(X,boolT,[(X,boolT)|_]) :- true.
var_type(X,boolT,[_|R]) :- var_type(X,boolT,R).

hastype(G,varT(X),intT) :- var_type(X,intT,G).
hastype(G,intT(X),intT) :- true.
hastype(G,varT(X),boolT) :- var_type(X,boolT,G).
hastype(G,boolT(X),boolT) :- true.

hastype(G,add(E1,E2),intT) :- hastype(G,E1,intT),hastype(G,E2,intT).
hastype(G,sub(E1,E2),intT) :- hastype(G,E1,intT),hastype(G,E2,intT).
hastype(G,mul(E1,E2),intT) :- hastype(G,E1,intT),hastype(G,E2,intT).
hastype(G,div(E1,E2),intT) :- hastype(G,E1,intT),hastype(G,E2,intT).
hastype(G,greater_than(E1,E2),boolT) :- hastype(G,E1,intT),hastype(G,E2,intT).
hastype(G,less_than(E1,E2),boolT) :- hastype(G,E1,intT),hastype(G,E2,intT).
hastype(G,equal(E1,E2),boolT) :- hastype(G,E1,intT),hastype(G,E2,intT).

hastype(G,and(E1,E2),boolT) :- hastype(G,E1,boolT),hastype(G,E2,boolT).
hastype(G,or(E1,E2),boolT) :- hastype(G,E1,boolT),hastype(G,E2,boolT).
hastype(G,not(E1),boolT) :- hastype(G,E1,boolT).