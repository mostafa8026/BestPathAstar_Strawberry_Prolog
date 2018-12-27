%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%                                     %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%       Artificial Intelligence       %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%       Project 1-C                   %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%                                     %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%       Designer: Mostafa             %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%                                     %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%                                     %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%       CopyRight 2010                %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% MAZE -------------------------------------------------------------------
cell(1, 1, '_'). cell(1, 2, '_'). cell(1, 3, '.').  cell(1, 4, '.').  cell(1, 5, '_').  cell(1, 6, '_').  cell(1, 7, '_').
cell(1, 8, '_'). cell(1, 9, '_'). cell(1, 10, '.'). cell(1, 11, '.'). cell(1, 12, '.'). cell(1, 13, '.'). cell(1, 14, '.').
cell(2, 1, '_'). cell(2, 2, '_'). cell(2, 3, '.').  cell(2, 4, '.').  cell(2, 5, '_').  cell(2, 6, '_').  cell(2, 7, '_').
cell(2, 8, '_'). cell(2, 9, '_'). cell(2, 10, '.'). cell(2, 11, '_'). cell(2, 12, '.'). cell(2, 13, '.'). cell(2, 14, '.').
cell(3, 1, '_'). cell(3, 2, '_'). cell(3, 3, '_').  cell(3, 4, '_').  cell(3, 5, '_').  cell(3, 6, '_').  cell(3, 7, '_').
cell(3, 8, '_'). cell(3, 9, '_'). cell(3, 10, '.'). cell(3, 11, '_'). cell(3, 12, '.'). cell(3, 13, '.'). cell(3, 14, '.').
cell(4, 1, '_'). cell(4, 2, '_'). cell(4, 3, '.').  cell(4, 4, '_').  cell(4, 5, '_').  cell(4, 6, '_').  cell(4, 7, '_').
cell(4, 8, '_'). cell(4, 9, '_'). cell(4, 10, '.'). cell(4, 11, '_'). cell(4, 12, '.'). cell(4, 13, '.'). cell(4, 14, '.').
cell(5, 1, '_'). cell(5, 2, '_'). cell(5, 3, '_').  cell(5, 4, '_').  cell(5, 5, 'X').  cell(5, 6, 'X').  cell(5, 7, 'X').
cell(5, 8, '_'). cell(5, 9, '_'). cell(5, 10, '_'). cell(5, 11, '_'). cell(5, 12, '_'). cell(5, 13, '_'). cell(5, 14, '_').
cell(6, 1, '_'). cell(6, 2, '_'). cell(6, 3, 'X').  cell(6, 4, '-').  cell(6, 5, '_').  cell(6, 6, '_').  cell(6, 7, 'X').
cell(6, 8, '_'). cell(6, 9, '_'). cell(6, 10, '_'). cell(6, 11, 'X'). cell(6, 12, '_'). cell(6, 13, '_'). cell(6, 14, '_').
cell(7, 1, '_'). cell(7, 2, '_'). cell(7, 3, 'X').  cell(7, 4, 'X').  cell(7, 5, '_').  cell(7, 6, '_').  cell(7, 7, 'X').
cell(7, 8, 'X'). cell(7, 9, 'X'). cell(7, 10, 'X'). cell(7, 11, '_'). cell(7, 12, '_'). cell(7, 13, '_'). cell(7, 14, '_').
cell(8, 1, '_'). cell(8, 2, '_'). cell(8, 3, '_').  cell(8, 4, '_').  cell(8, 5, '_').  cell(8, 6, '_').  cell(8, 7, '_').
cell(8, 8, '_'). cell(8, 9, 'X'). cell(8, 10, '_'). cell(8, 11, 'X'). cell(8, 12, '.'). cell(8, 13, '.'). cell(8, 14, '.').
cell(9, 1, '_'). cell(9, 2, '_'). cell(9, 3, '.').  cell(9, 4, '.').  cell(9, 5, '_').  cell(9, 6, '_').  cell(9, 7, '_').
cell(9, 8, '_'). cell(9, 9, '_'). cell(9, 10, '.'). cell(9, 11, '.'). cell(9, 12, '.'). cell(9, 13, '.'). cell(9, 14, '.').
cell(10, 1, 'X'). cell(10, 2, '_'). cell(10, 3, '_').  cell(10, 4, '_').  cell(10, 5, '_').  cell(10, 6, '.').  cell(10, 7, '.').
cell(10, 8, '.'). cell(10, 9, '_'). cell(10, 10, '_'). cell(10, 11, '_'). cell(10, 12, '.'). cell(10, 13, '.'). cell(10, 14, '.').
cell(11, 1, '_'). cell(11, 2, '_'). cell(11, 3, '_').  cell(11, 4, '_').  cell(11, 5, '_').  cell(11, 6, '.').  cell(11, 7, '.').
cell(11, 8, '.'). cell(11, 9, '_'). cell(11, 10, '_'). cell(11, 11, '_'). cell(11, 12, '.'). cell(11, 13, '.'). cell(11, 14, '.').
%-------------------------------------------------------------------------

%is the cell in list?-----------------------------------------------------
ismember(H,[H|T]).
ismember(H,[H2|T]):-ismember(H,T).

%is the cell an obstacle? ------------------------------------------------
isObstacle(A, B) :- cell(A, B, 'X'); cell(A, B, '.').

%bound of the maze matrix ------------------------------------------------
bounded(A,B):-A<12,A>0,B<15,B>0.

% moving in 4 direction --------------------------------------------------
move(A,B,C,D):-C is A-1,D is B.
move(A,B,C,D):-C is A+1,D is B.
move(A,B,C,D):-C is A,D is B-1.
move(A,B,C,D):-C is A,D is B+1.

%heuristic function = manhattan distance ---------------------------------
h(A,B,C,D,E) :- E is abs(A-C) + abs(B-D).

% use in findAll ---------------------------------------------------------
f(cellP(cell(A,B, _, _),_), cellP(cell(C,D, _, _),_), Closed, Fringe):-
move(A, B, C, D),
not(ismember(cell(C,D), Closed)),
not(isObstacle(C,D)),
bounded(C,D).

%successor = give me neighbor cells and recognize obstacle and in closed cells with findall methode.
successors(SA, SB, GA, GB, G,Parent, S, Closed, Fringe, L):-
G1 is G+1,
findall(cellP(cell(C,D, _, G1),[cell(SA,SB)|Parent]), f(cellP(cell(SA,SB,_, _),_), cellP(cell(C,D,_, G1),[Parent|P]), Closed, Fringe), S),
updateS(S, GA, GB, L).

%update cells with new f .------------------------------------------------
updateS([],GA,GB,[]).
updateS([cellP(cell(A,B,_,G),P)|S], GA, GB, [cellP(cell(A,B,F1,G),P)|L]):-
h(A,B,GA,GB,E),
F1 is G+E,
updateS(S, GA, GB, L).

%ordinery search----------------------------------------------------------
insertO(X, [], [X]).
insertO(X, [A|B], [X,A|B]).

% insertion Sort ---------------------------------------------------------
insert(X, [], [X]).
insert(cellP(cell(A1,B1,F1,G1),P1), [cellP(cell(A2,B2,F2,G2),P2)|L1], [cellP(cell(A1,B1,F1,G1),P1),cellP(cell(A2,B2,F2,G2),P2)|L1]):- F1=<F2.
insert(cellP(cell(A1,B1,F1,G1),P1), [cellP(cell(A2,B2,F2,G2),P2)|L1], [cellP(cell(A2,B2,F2,G2),P2)|L2]):- F1>F2, insert(cellP(cell(A1,B1,F1,G1),P1),L1,L2).

%insert an array in another Array and return a sorted array.--------------
insertArray([], S, S).
insertArray([A], S, D):-insert(A, S, D).
insertArray([A|B], S, E):- insert(A, S, D), insertArray(B, D, E).

%reverse Array
reverse([],D,D).
reverse([H1|H],D,F):-insertO(H1, D, E),reverse(H, E, F).

%array length
length([], 0).
length([A|B], G1):-length(B, G), G1 is G+1.

%first Element Of array.
firstElement([A], A).
firstElement([A|B], A).

%lastElement Of Array
lastElement([A], A).
lastElement([A|B], C):- lastElement(B, C).

%print--------------------------------------------------------------------
%return cell , if a path return * else return cell(A,B,E)=>E
ret(A,B,M,'*'):-ismember(cell(A,B),M).
ret(A,B,M,E):-cell(A,B,F),E is F.

%fact
print(12, _,_).

%goto next row
print(A,14,P):-
A1 is A+1,
write(A),
nl,
print(A1,0,P).

%print row.
print(A,B,P):-
B1 is B+1,
ret(A,B1,P,W),
write(W),
write(" "),
print(A, B1,P).

%search in the maze : using successor methode and insertArray methode.----
%When The solution Not Found , the Fringe is empty
search([], _, _):-write("not Found"), nl.

%write The Solution
search([cellP(cell(GA,GB,_,_),P)|_],cell(GA,GB), C):-
insertO(cell(GA,GB), P, M), reverse(M, D, E), firstElement(E, FE), 
write("---------------------------------------------------------------------------"),nl,
write("from "), write(FE), write(" To "), lastElement(E, LE), write(LE),
write(" in "), length(E, Len), write(Len-1), write(" Moves. "), nl,
write(E),nl,
write("---------------------------------------------------------------------------"),nl,nl,
print(1,0, E).

search([cellP(cell(SA,SB,F,G),P)|Fringe],cell(GA,GB), Closed):-
not(isObstacle(SA, SB)),
not(isObstacle(GA, GB)),
successors(SA,SB,GA,GB,G,P, S, Closed, Fringe, L),
insertArray(L, Fringe, Y),
search(Y, cell(GA,GB), [cell(SA,SB)|Closed]).


%solve the program with start and goal node.------------------------------
solve(cell(SA,SB), cell(GA,GB)):-
insert(cellP(cell(SA,SB,0,0),[]),[], Fringe),
search(Fringe, cell(GA,GB), []).

%random Numbers
rand1(SA,SB):-
SA1 is random(11), SB1 is random(14), SA is SA1+1, SB is SB1+1.
rand2(GA,GB):-
GA1 is random(11), GB1 is random(14), GA is GA1+1, GB is GB1+1.
%give start cell first and the goal cell after.---------------------------
?-rand1(SA,SB),rand2(GA,GB),solve(cell(SA,SB), cell(GA,GB)).
