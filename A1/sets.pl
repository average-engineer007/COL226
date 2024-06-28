del(_, [ ] , [ ]) :- !.

del(X, [X|R], Z) :- del(X, R, Z), !.

del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.



remdups([ ], [ ]) :- !.

remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).



unionI([ ], S2, S2) :- !.

unionI(S1, [ ], S1) :- !.

unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).

/*
?- unionI([],[],X).
X = [].

?- unionI([1,2,3,4],[2,1,4,3],X).
X = [1,2,3,4].

?- unionI([1,2,3,4],[2,1,5,4,3],X).
X = [1,2,3,4,5].

?- unionI([1,2,3,4],[],X).
X = [1,2,3,4].

?- unionI([],[2,1,4,3],X).
X = [2,1,4,3].

?- unionI([5,6,7,8],[1,2,3,4],X).
X = [5,6,7,8,1,2,3,4].

?- unionI([5,6,7,8,2,3],[1,2,3,4],X).
X = [5,6,7,8,2,3,1,4].

*/
  
append( [ ], L, L).

append( [X|R], L, [X|Z]) :- append(R, L, Z).



mapcons(_, [ ], [ ]) :- !.

mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).


mapconst(_,[],[]):-!.
mapconst(X, [Y|R], [ (X,Y) | Z ]) :- mapconst(X, R, Z).



powerI([ ], [ [ ] ]) :- !.

powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).

/*
?- powerI([],X).
X=[[]].

?- powerI([1],X).
X=[[1],[]].

?- powerI([11,21],X).
X=[[11,21],[11],[21],[]].

?- powerI([3,2,1],X).
X=[[3,2,1],[3,2],[3,1],[3],[2,1],[2],[1],[]].

*/



diffl([],_,[]) :-!.
diffl(S1,[],S1) :-!.
diffl(S1,[X|R],S3) :- diffl(S1,R,T),del(X,T,S3).

/*
?- diffl([],[],X).
X = [].

?- diffl([1,2],[],X).
X = [1,2].

?- diffl([1,2,3,4],[2,4,1,3],X).
X = [].

?- diffl([1],[2,3,1,4],X).
X = [].

?- diffl([6,3,2,5,1,4],[2,4],X).
X=[6,3,5,1].

?- diffl([1,2,3],[4,5,6],X).
X = [1,2,3].
*/

cartesianl([],_S2,[]):-!.
cartesianl(_,[],[]):-!.
cartesianl([X|R],S2,S3):- cartesianl(R,S2,T) , mapconst(X, S2, P), append(P, T, S3).

/*
?- cartesianl([1,2],[1,2],X).
X=[[1,1],[1,2],[2,1],[2,2]].

?- cartesianl([1,2],[3,4],X).
X=[[1,3],[1,4],[2,3],[2,4]].

?- cartesianl([1],[3,4,5,6],X).
X = [[1,3],[1,4],[1,5],[1,6]].

*/


mem(_,[]) :- fail.
mem(X,[X|_]) :-true.
mem(X,[_|R]) :- mem(X,R).



interl([],_,[]):-!.
interl(_,[],[]):-!.
interl([X|R],S2,[X|Z]):-mem(X,S2) , interl(R,S2,Z),!.
interl([_|R],S2,S3):-interl(R,S2,S3).
/*
?- interl([],[1,2,3],X).
X = [].

?- interl([1,2,3,4],[2,4,1,3],X).
X = [1,2,3,4].

?- interl([1],[2,3,1,4],X).
X = [1].

?- interl([1,2,3,4],[3,4,5,6],X).
X = [3,4].

?- interl([1,2,3],[4,5,6],X).
X = [].
*/



subset([],_) :- !.
subset([X|R],L) :- mem(X,L), subset(R,L).
eqset(L1, L2) :- subset(L1,L2), subset(L2,L1).

/* to check two power sets*/
memt(_,[]):-fail.
memt(X,[Y|_]):-eqset(X,Y),!.
memt(X,[_|R]):-memt(X,R).
subsetp([],_) :- !.
subsetp([X|R],L) :- memt(X,L), subsetp(R,L).

eqpowerset(L1, L2) :- subsetp(L1,L2), subsetp(L2,L1).




mem( (X,X), refclos(R,S)) :- mem(X,S), !.
mem( (X,Y), refclos(R,S)) :- mem((X,Y), R), !.

mem((X,Y), symclos(R)) :- mem((X,Y), R), !.
mem((X,Y), symclos(R)) :- mem((Y,X), R),!.


mem((X,Z), comp(R1, R2)) :- mem((X,Y),R1),mem((Y,Z),R2).

mem((X,Y),transclos(R)) :- mem((X,Y),R),!.
mem((X,Z),transclos(R)) :- mem((X,Y),R),del((X,Y),R,F),mem((Y,Z),transclos(F)).

mem((X,X),reftransclos(R,S)) :- mem(X,S),!.
mem((X,Y),reftransclos(R,S)) :- mem((X,Y),R),!.
mem((X,Z),reftransclos(R,S)) :- mem((X,Y),R),del((X,Y),R,F),mem((Y,Z),transclos(F)).

/*
?- mem((1,3),reftransclos([(1,2),(2,3)],[1,2,3])).
true .

?- mem((1,5),reftransclos([(1,2),(2,3)],[1,2,3])).
false.

?- mem((1,2),reftransclos([(1,2),(2,1)],[1,2])).
true .

?- mem((1,1),reftransclos([(1,2),(2,3)],[1,2])).
true .

?- mem((1,2),reftransclos([(1,1),(2,3),(1,3)],[1,2,3])).
false .

?- mem((1,2),reftransclos([(1,1),(3,2),(1,3)],[1,2,3])).
true .

?- mem((1,5),reftransclos([(1,2),(2,3),(3,1)],[1,2,3])).
false .

?- mem((1,5),reftransclos([(1,1),(1,2),(1,3),(1,4),(4,5)],[1,2,3,4,5])).
true .
*/



mem((X,X),eq(R,S)) :- mem(X,S).
mem((X,Z),eq(R,S)) :-mem((X,Y),R),del((X,Y),R,F),(mem((Y,Z),eq(F,S));mem((Z,Y),eq(F,S))).
mem((X,Z),eq(R,S)) :- mem((Y,X),R),del((Y,X),R,F),(mem((Z,Y),eq(F,S));mem((Y,Z),eq(F,S))).
/*
?- mem((1,3),eq([(1,2),(2,3)],[1,2,3])).
true .

?- mem((1,5),eq([(1,2),(2,3)],[1,2,3])).
false.

?- mem((1,5),eq([(1,2),(2,3),(4,5),(1,3)],[1,2,3,4,5])).
false.

?- mem((1,4),eq([(1,2),(2,3),(3,1),(4,5)],[1,2,3,4,5])).
false.

?- mem((1,4),eq([(1,2),(2,3),(3,1),(4,5)],[1,2,3,4,5])).
false.

?- mem((1,5),eq([(1,3),(2,3),(5,6),(4,6),(2,4)],[1,2,3,4,5,6])).
true.

?- mem((1,6),eq([(6,5),(5,4),(4,3),(3,2),(2,1)],[1,2,3,4,5,6])).
true.
*/