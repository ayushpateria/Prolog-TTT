% @author Aysuh Pateria <cs15btech11007@iith.ac.in>
% Tic tac toe program witten in prolog. It uses the minmax algorithm to calculate next move.

:-initialization(play).

play :- intro, playfrom([0,0,0,0,0,0,0,0,0]).

intro :-
  write('The numbers representation are as follows:\n0 - Blank\n1 - User\n2 - Computer\nType a period after every input you give.'),
  nl,
  displayBoard([0,0,0,0,0,0,0,0,0]).

playfrom(Board) :- win(Board, 1), write('You win!').
playfrom(Board) :- win(Board, 2), write('I win!').
playfrom(Board) :- \+(areMovesLeft(Board)), write('Draw!'). 
playfrom(Board) :- write('Enter row number: '), read(R), write('Enter column number: '), read(C), N is C + (R-1)*3,
  xmove(Board, N, Newboard), 
  displayBoard(Newboard),
  orespond(Newboard, Newnewboard), 
  displayBoard(Newnewboard),
  playfrom(Newnewboard).

displayBoard([A,B,C,D,E,F,G,H,I]) :- write([A,B,C]),nl,write([D,E,F]),nl,
 write([G,H,I]),nl,nl.

win(Board, P) :- rowwin(Board, P).
win(Board, P) :- colwin(Board, P).
win(Board, P) :- diagwin(Board, P).

rowwin(Board, P) :- Board = [P,P,P,_,_,_,_,_,_].
rowwin(Board, P) :- Board = [_,_,_,P,P,P,_,_,_].
rowwin(Board, P) :- Board = [_,_,_,_,_,_,P,P,P].

colwin(Board, P) :- Board = [P,_,_,P,_,_,P,_,_].
colwin(Board, P) :- Board = [_,P,_,_,P,_,_,P,_].
colwin(Board, P) :- Board = [_,_,P,_,_,P,_,_,P].

diagwin(Board, P) :- Board = [P,_,_,_,P,_,_,_,P].
diagwin(Board, P) :- Board = [_,_,P,_,P,_,P,_,_].



orespond(Board,Board) :- \+(areMovesLeft(Board)).

orespond(Board,Newboard) :- MI is 0, Best is -1000,
playIndex(Board, 0, MI, Best, Ans), A is (Ans+1),!,
omove(Board, A, Newboard).


playIndex(Board, 9, MI, Best, Ans) :- Ans is MI, !.
playIndex(Board, I, MI, Best, Ans) :-  match(I, Board, El), El=0, I2 is I+1, omove(Board, I2, Newboard), minimax(Newboard, 1, V),  (V > Best -> NewBest is V, NewMI is I; NewBest is Best , NewMI is MI), playIndex(Board, I2, NewMI, NewBest, Ans),!.
playIndex(Board, I, MI, Best, Ans) :-  match(I, Board, El), \+(El=0), NI is I + 1, playIndex(Board, NI, MI, Best, Ans).


xmove([0,B,C,D,E,F,G,H,I], 1, [1,B,C,D,E,F,G,H,I]).
xmove([A,0,C,D,E,F,G,H,I], 2, [A,1,C,D,E,F,G,H,I]).
xmove([A,B,0,D,E,F,G,H,I], 3, [A,B,1,D,E,F,G,H,I]).
xmove([A,B,C,0,E,F,G,H,I], 4, [A,B,C,1,E,F,G,H,I]).
xmove([A,B,C,D,0,F,G,H,I], 5, [A,B,C,D,1,F,G,H,I]).
xmove([A,B,C,D,E,0,G,H,I], 6, [A,B,C,D,E,1,G,H,I]).
xmove([A,B,C,D,E,F,0,H,I], 7, [A,B,C,D,E,F,1,H,I]).
xmove([A,B,C,D,E,F,G,0,I], 8, [A,B,C,D,E,F,G,1,I]).
xmove([A,B,C,D,E,F,G,H,0], 9, [A,B,C,D,E,F,G,H,1]).
xmove(Board, N, Board) :- write('Illegal move.'), nl.


omove([0,B,C,D,E,F,G,H,I], 1, [2,B,C,D,E,F,G,H,I]).
omove([A,0,C,D,E,F,G,H,I], 2, [A,2,C,D,E,F,G,H,I]).
omove([A,B,0,D,E,F,G,H,I], 3, [A,B,2,D,E,F,G,H,I]).
omove([A,B,C,0,E,F,G,H,I], 4, [A,B,C,2,E,F,G,H,I]).
omove([A,B,C,D,0,F,G,H,I], 5, [A,B,C,D,2,F,G,H,I]).
omove([A,B,C,D,E,0,G,H,I], 6, [A,B,C,D,E,2,G,H,I]).
omove([A,B,C,D,E,F,0,H,I], 7, [A,B,C,D,E,F,2,H,I]).
omove([A,B,C,D,E,F,G,0,I], 8, [A,B,C,D,E,F,G,2,I]).
omove([A,B,C,D,E,F,G,H,0], 9, [A,B,C,D,E,F,G,H,2]).


areMovesLeft(Board) :- member(0, Board).

evaluate(Board, 1, 10) :- rowwin(Board, 1),!.
evaluate(Board, 1, -10) :- rowwin(Board, 2),!.
evaluate(Board, 1, 10) :- colwin(Board, 1),!.
evaluate(Board, 1, -10) :- colwin(Board, 2),!.
evaluate(Board, 1, 10) :- diagwin(Board, 1),!.
evaluate(Board, 1, -10) :- diagwin(Board, 2),!.

evaluate(Board, 2, -10) :- rowwin(Board, 1),!.
evaluate(Board, 2, 10) :- rowwin(Board, 2),!.
evaluate(Board, 2, -10) :- colwin(Board, 1),!.
evaluate(Board, 2, 10) :- colwin(Board, 2),!.
evaluate(Board, 2, -10) :- diagwin(Board, 1),!.
evaluate(Board, 2, 10) :- diagwin(Board, 2),!.

evaluate(Board, N, 0).

minimax(Board, P, V) :- evaluate(Board, 2, A), \+(A=0), V is A, !.
minimax(Board, P, V) :- \+(areMovesLeft(Board)), V is 0, !.

minimax(Board, 1, V) :-  Best is 1000, loopAll(Board, 1, 0, Best, Ans), V is Ans,!.

minimax(Board, 2, V) :-  Best is -1000, loopAll(Board, 2, 0, Best, Ans), V is Ans,!.

% minimizer move.
loopAll(Board, 1, 9, Best, Ans) :- Ans is Best, !.
loopAll(Board, 1, I, Best, Ans) :- match(I, Board, El), El=0, I2 is I+1, xmove(Board, I2, Newboard),minimax(Newboard, 2, V), NewBest is min(Best, V), loopAll(Board, 1, I2, NewBest, Ans).
loopAll(Board, 1, I, Best, Ans) :- match(I, Board, El), \+(El=0), NI is I+1,  loopAll(Board, 1, NI, Best, Ans).

% maximizer move.
loopAll(Board, 2, 9, Best, Ans) :- Ans is Best,!.
loopAll(Board, 2, I, Best, Ans) :- match(I, Board, El), El=0, I2 is I+1, omove(Board, I2, Newboard),  minimax(Newboard, 1, V),  NewBest is max(Best, V), loopAll(Board, 2, I2, NewBest, Ans).
loopAll(Board, 2, I, Best, Ans) :- match(I, Board, El), \+(El=0), NI is I+1,  loopAll(Board, 2, NI, Best, Ans).

% match function

match(0, [Head|_], Head) :- !.

match(N, [_|Tail], Elem) :-
    nonvar(N),
    M is N-1,
    match(M, Tail, Elem).

match(N,[_|T],Item) :-       % Clause added KJ 4-5-87 to allow mode
    var(N),         % match(-,+,+)
    match(M,T,Item),
    N is M + 1.