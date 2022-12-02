%my_agent.pl

%   this procedure requires the external definition of two procedures:
%
%     init_agent: called after new world is initialized.  should perform
%                 any needed agent initialization.
%
%     run_agent(percept,action): given the current percept, this procedure
%                 should return an appropriate action, which is then
%                 executed.
%
% This is what should be fleshed out



remover( _, [], []).
remover( R, [R|T], T2) :- remover( R, T, T2).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).


incr(X, X1):-
  X1 is X+1.

decr(X, X1):-
  X1 is X-1.

initZero(X):-
  X is 0.

%MEMORY STORAGE DECLARATIONS
:- dynamic
  safeTiles/1,       %list of all the safe square                      (LIST)
  canMakeSafeMove/0, %set to false by default                          (BOOL)
  foundWumpus/0,
  wumpusLocation/1,
  currentSquare/1,   %cordinates of the square the agent is current at (LIST)
  facing/1,          %which direction the agent is facing (n,e,s,w)    (LIST)
  visited/1,         %list of all the squares the agent has visited    (LIST)
  actions/1,
  outOfBound/0,      %if the agent is out of bound                     (BOOL)
  frontier/3.        %keeps track of adjacent unvisited tiles and their danger levels


%RESET AGENT
clearAgent:-
  retractall(safeTiles(_)),
  retractall(canMakeSafeMove),
  retractall(foundWumpus),
  retractall(outOfBound),
  retractall(currentSquare(_)),
  retractall(facing(_)),
  retractall(visited(_)),
  retractall(actions(_)),
  retractall(wumpusLocation(_)),
  retractall(frontier(_,_,_)).

%INITIALIZE AGENT
init_agent:-
  format('\n====================================================I\n'),
  format('This is init_agent:\n\tIt gets called once, use it for your initialization\n\n'),


  clearAgent,

  asserta(actions(enterDungeon)),
  asserta(currentSquare([1,1])),
  asserta(safeTiles([1,1])),
  asserta(visited([1,1])),
  asserta(facing(east)),
  %asserta(frontier([2,1],0,0)), %cords, pit level, wumpus level
  %asserta(frontier([1,2],0,0)),

  assert(actions(turnleft)),
  assert(actions(goforward)),
  assert(actions(turnright)),
  assert(actions(goforward)),

  assert(actions(turnleft)),

  assert(actions(goforward)),
  assert(actions(grab)),

  assert(actions(turnleft)),
  assert(actions(turnleft)),
  assert(actions(goforward)),
  assert(actions(turnright)),


  assert(actions(goforward)),
  assert(actions(turnleft)),
  assert(actions(goforward)),

  assert(actions(climb)),


  format('====================================================I\n\n').


%GET FUNCTIONS
%GET LAST ACTION: returns the last action the agent took
getLastAction(Action):-
  actions(Action), !.

%GET CURRENT POSITION: returns the current postition of the agent
getCurrentPosition(Pos):-
  currentSquare(Pos), !.

%GET CURRENT FACING DIRECTION: returns the current direction the agent is facing
getCurrentDirection(Dir):-
  facing(Dir), !.

%GET CLOSEST SAFE SPOT
nearestSafe(Spot):- 
  safeTiles(Spot), !.

getNextMove(Action):-
  retract(actions(A)), !,
  actions(Action), !.

printAllFrontier:-  
  forall((frontier(D,-1,-1)), format('[~w,~w]  Safe\n', D)),
  forall((frontier(D,B,W), (B+W =\= -2)), format('~w  Pit: ~w  Wump: ~w\n', [D,B,W])).

printAllVisited:-
  forall(visited(V), format('[~w,~w] ', V)).


addVisited(V):-
  \+visited(V),
  assert(visited(V)).

movedToNewTile(Action, Position):-
  Action = goforward,
  \+visited(Position).



%CHANGE WHICH DIRECTION AGENT IS FACING
lookLeft(east, north).
lookLeft(north, west).
lookLeft(west, south).
lookLeft(south, east).

lookRight(east, south).
lookRight(north, east).
lookRight(west, north).
lookRight(south, west).


%MOVEMENT FUNCTIONS
nextSquare([X,Y], north, [X1,Y1]) :- X1 is X, Y1 is Y + 1.
nextSquare([X,Y], east, [X1,Y1]) :- X1 is X + 1, Y1 is Y.
nextSquare([X,Y], south, [X1,Y1]) :- X1 is X, Y1 is Y - 1.
nextSquare([X,Y], west, [X1,Y1]) :- X1 is X - 1, Y1 is Y.

updatePosition([_,_,_,Bump,_], Action, Position, Direction):-
  Action = goforward, Bump=no, nextSquare(Position, Direction, NewPosition), asserta(currentSquare(NewPosition)), updateCurrentTileThreat(NewPosition);
  Action = goforward, Bump=yes;
  Action = turnleft, lookLeft(Direction, NewDirection), asserta(facing(NewDirection));
  Action = turnright, lookRight(Direction, NewDirection), asserta(facing(NewDirection));
  Action = climb;
  Action = grab;
  Action = enterDungeon.

updateCurrentTileThreat(Position):-
  [X,Y] = Position,
  ((retract(frontier([X,Y], _, _)); true),
  assert(frontier([X,Y], -1, -1))).


%update frontier
updateFrontier(Cords):-
  [X,Y] = Cords,
  incr(X,X2),
  decr(X,X3),
  incr(Y,Y2),
  decr(Y,Y3),
  ((frontier([1,3],A,B), format('HELPPPP ~w ~w\n',[A,B]));true), 
  ((\+frontier([X2,Y],_,_), assert(frontier([X2,Y],0,0)));true),
  ((\+frontier([X3,Y],_,_), assert(frontier([X3,Y],0,0)));true),
  ((\+frontier([X,Y2],_,_), assert(frontier([X,Y2],0,0)));true),
  ((\+frontier([X,Y3],_,_), assert(frontier([X,Y3],0,0)));true).


%if the tile has not been visited, mark it as visited
haveNotVisited(CurrentPos):-
  \+visited(CurrentPos),
  addVisited(CurrentPos),
  updateFrontier(CurrentPos),
  assert(safeTiles(CurrentPos)).

%should the agent asses the threat at this tile.
%if the tile has already been visited then no, return true.
%if the tile has not been visited then yes, asses the threats.
shouldAgentAsses(Cords, Percepts):-
  \+haveNotVisited(Cords);         %has been visited
  assessThreat(Percepts, Cords).




%ACTION FUNCTIONS
updateAction(Percepts, Action, Position, Facing):-  
  moveTo(Position, Facing).




%ASSES THREAT  [Stench, Breeze, Glitter, Bump, Scream]
  % -1 means there is no possibility for that threat on that tile
  % 0 means that there is no data on the tile yet
  % 1-4 means the likelyhood of death on that tile (4 highest, 1 lowest) 
  %Should only update the surrounding tiles, if they have not already been visited

%assessThreat([_, _, yes, _, _], _). %glitter
%assessThreat([_, _, _, yes, _], _). %bump
%assessThreat([_, _, _, _, yes], CurrentPos):- %scream 
  %replace every threatLevel list element to have a wumpus threat level of 0%

%NO DANGER
assessThreat([no, no, _, _, _], CurrentPos):-
    [X,Y] = CurrentPos,
    %WTL stands for Wumpus Threat Level
    %PTL stands for Pit Threat Level

    %update the threat levels for the square to the right
    incr(X,X2),
    (\+visited([X2,Y]), ( (retract(frontier([X2,Y], _, _)), assert(frontier([X2,Y], -1, -1))); assert(frontier([X2,Y], -1, -1)) );true),

    %update the threat levels for the square above
    incr(Y,Y2), 
    (\+visited([X,Y2]), ( (retract(frontier([X,Y2], _, _)), assert(frontier([X,Y2], -1, -1))); assert(frontier([X,Y2], -1, -1)) );true),

    %update the threat levels for the square to the left
    decr(X,X22),
    (\+visited([X22,Y]), ( (retract(frontier([X22,Y], _, _)), assert(frontier([X22,Y], -1, -1))); assert(frontier([X22,Y], -1, -1)) );true),

    %update the threat levels for the square below
    decr(Y,Y22),
    (\+visited([X,Y22]), ( (retract(frontier([X,Y22], _, _)), assert(frontier([X,Y22], -1, -1))); assert(frontier([X,Y22], -1, -1)) );true). 


%BREEZE
assessThreat([no, yes, _, _, _], CurrentPos):-  
  [X,Y] = CurrentPos,
  %WTL stands for Wumpus Threat Level
  %PTL stands for Pit Threat Level
  %update the threat levels for the square to the right
  incr(X,X2),
  (\+visited([X2,Y]),
  (
    (retract(frontier([X2,Y], -1, _)), assert(frontier([X2,Y], -1, -1)));
    ((retract(frontier([X2,Y], PTL1, _)), incr(PTL1, NewPTL1)); NewPTL1 is 1), 
    assert(frontier([X2,Y], NewPTL1, -1))
  );true),

  %update the threat levels for the square above
  incr(Y,Y2), 
  (\+visited([X,Y2]),
  (
    (retract(frontier([X,Y2], -1, _)), assert(frontier([X,Y2], -1, -1)));
    ((retract(frontier([X,Y2], PTL2, _)), incr(PTL2, NewPTL2)); NewPTL2 is 1), 
    assert(frontier([X,Y2], NewPTL2, -1))
  );true),

  %update the threat levels for the square to the left
  decr(X,X22),
  (\+visited([X22,Y]),
  (
    (retract(frontier([X22,Y], -1, _)), assert(frontier([X22,Y], -1, -1)));
    ((retract(frontier([X22,Y], PTL3, _)), incr(PTL3, NewPTL3)); NewPTL3 is 1), 
    assert(frontier([X22,Y], NewPTL3, -1))
  );true),

  %update the threat levels for the square below
  decr(Y,Y22),
  (\+visited([X,Y22]),
  (
    (retract(frontier([X,Y22], -1, _)), assert(frontier([X,Y22], -1, -1)));
    ((retract(frontier([X,Y22], PTL4, _)), incr(PTL4, NewPTL4)); NewPTL4 is 1), 
    assert(frontier([X,Y22], NewPTL4, -1))
  );true).



%STENCH 
assessThreat([yes, no, _, _, _], CurrentPos):- 
  [X,Y] = CurrentPos,
  %WTL stands for Wumpus Threat Level
  %PTL stands for Pit Threat Level
  %update the threat levels for the square to the right
  ((foundWumpus, format('FOUND WUMPUS!!\n')); format('Wumpus Hidden\n')),

  incr(X,X2),
  (\+visited([X2,Y]),
  (
    (retract(frontier([X2,Y], _, -1)), assert(frontier([X2,Y], -1, -1)));
    ((\+foundWumpus, retract(frontier([X2,Y], _, WTL1)), incr(WTL1, NewWTL1)); ((\+foundWumpus, NewWTL1 is 1); NewWTL1 is -1)), 
    ((NewWTL1 > 1, assert(foundWumpus), assert(wumpusLocation([X2,Y])), assert(frontier([X2,Y], -1, NewWTL1)));
    assert(frontier([X2,Y], -1, NewWTL1)))
  );true),

  %update the threat levels for the square above
  incr(Y,Y2), 
  (\+visited([X,Y2]),
  ( 
    (retract(frontier([X,Y2], _, -1)), assert(frontier([X,Y2], -1, -1)));
    ((\+foundWumpus, retract(frontier([X,Y2], _, WTL2)), incr(WTL2, NewWTL2)); ((\+foundWumpus, NewWTL2 is 1); NewWTL2 is -1)),
    ((NewWTL2 > 1, assert(foundWumpus), assert(wumpusLocation([X,Y2])), assert(frontier([X,Y2], -1, NewWTL2))); 
    assert(frontier([X,Y2], -1, NewWTL2)))
  );true),

  %update the threat levels for the square to the left
  decr(X,X22),
  (\+visited([X22,Y]),
  (
    (retract(frontier([X22,Y], _, -1)), assert(frontier([X22,Y], -1, -1)));
    ((\+foundWumpus, retract(frontier([X22,Y], _, WTL3)), incr(WTL3, NewWTL3)); ((\+foundWumpus, NewWTL3 is 1); NewWTL3 is -1)), 
    ((NewWTL3 > 1, assert(foundWumpus), assert(wumpusLocation([X22,Y])), assert(frontier([X22,Y], -1, NewWTL3)));
    assert(frontier([X22,Y], -1, NewWTL3)))
  );true),

  %update the threat levels for the square below
  decr(Y,Y22),
  (\+visited([X,Y22]),
  (
    (retract(frontier([X,Y22], _, -1)), assert(frontier([X,Y22], -1, -1)));
    ((\+foundWumpus, retract(frontier([X,Y22], _, WTL4)), incr(WTL4, NewWTL4)); ((\+foundWumpus, NewWTL4 is 1); NewWTL4 is -1)),
    ((NewWTL4 > 1, assert(foundWumpus), assert(wumpusLocation([X,Y22])), assert(frontier([X,Y22], -1, NewWTL4))); 
    assert(frontier([X,Y22], -1, NewWTL4)))
  );true).

%STENCH AND BREEZE
assessThreat([yes, yes, _, _, _], CurrentPos):-  
  [X,Y] = CurrentPos,
  %WTL stands for Wumpus Threat Level
  %PTL stands for Pit Threat Level
  %update the threat levels for the square to the right
  incr(X,X2),
  (\+visited([X2,Y]),
  (
    (frontier([X2,Y], -1, -1)); %if this square is safe don't touch it
    ((\+foundWumpus, retract(frontier([X2,Y], -1, W)), incr(W, W2), assert(frontier([X2,Y], -1, W2)), (W2 > 1, assert(foundWumpus), assert(wumpusLocation([X2,Y]))));true), %incr the W if we know a pit isnt there
    ((retract(frontier([X2,Y], P, -1)), incr(P, P2), assert(frontier([X2,Y], P2, -1)));true),                                                             %incr the P threat if we know wumpus isnt there
    ((retract(frontier([X2,Y], PTL1, WTL1)), incr(PTL1, NewPTL1), ((\+foundWumpus, incr(WTL1, NewWTL1)); NewWTL1 is -1)); (NewPTL1 is 1, NewWTL1 is 1)),  %initialize the threat values
    ((NewWTL1 > 1, assert(foundWumpus), assert(wumpusLocation([X2,Y])), assert(frontier([X2,Y], NewPTL1, NewWTL1))); assert(frontier([X2,Y], NewPTL1, NewWTL1))) %assert
  );true),

  %update the threat levels for the square above
  incr(Y,Y2), 
  (\+visited([X,Y2]),
  (
    (frontier([X,Y2], -1, -1)); %if this square is safe don't touch it
    (\+foundWumpus, retract(frontier([X,Y2], -1, W1)), incr(W1, W12), assert(frontier([X,Y2], -1, W12)), (W12 > 1, assert(foundWumpus), assert(wumpusLocation([X,Y2])));true),
    ((retract(frontier([X,Y2], P1, -1)), incr(P1, P12), assert(frontier([X,Y2], P12, -1)));true),
    ((retract(frontier([X,Y22], PTL2, WTL2)), incr(PTL2, NewPTL2), ((\+foundWumpus, incr(WTL2, NewWTL2)); NewWTL2 is -1)); (NewPTL2 is 1, NewWTL2 is 1)), 
    ((NewWTL2 > 1, assert(foundWumpus), assert(wumpusLocation([X,Y2])), assert(frontier([X,Y2], NewPTL2, NewWTL2))); assert(frontier([X,Y2], NewPTL2, NewWTL2)))
  );true),

  %update the threat levels for the square to the left
  decr(X,X22),
  (\+visited([X22,Y]),
  (
    ((frontier([X22,Y], -1, -1)); (frontier([X22,Y], _, 2))); %if this square is safe don't touch it
    ((\+foundWumpus, retract(frontier([X22,Y], -1, W3)), incr(W3, W32), assert(frontier([X22,Y], -1, W32)), (W32 > 1, assert(foundWumpus), assert(wumpusLocation([X22,Y]))));true),
    ((retract(frontier([X22,Y], P3, -1)), incr(P3, P32), assert(frontier([X22,Y], P32, -1)));true),
    ((retract(frontier([X22,Y], PTL3, WTL3)), incr(PTL3, NewPTL3), ((\+foundWumpus, incr(WTL3, NewWTL3)); NewWTL3 is -1)); (NewPTL3 is 1, NewWTL3 is 1)), 
    ((NewWTL3 > 1, assert(foundWumpus), assert(wumpusLocation([X22,Y])), assert(frontier([X22,Y], NewPTL3, NewWTL3))); assert(frontier([X22,Y], NewPTL3, NewWTL3)))
  );true),

  %update the threat levels for the square below
  decr(Y,Y22),
  (\+visited([X,Y22]),
  (
    (frontier([X,Y22], -1, -1)); %if this square is safe don't touch it
    ((\+foundWumpus, retract(frontier([X,Y22], -1, W4)), incr(W4, W42), assert(frontier([X,Y22], -1, W42)), (W42 > 1, assert(foundWumpus), assert(wumpusLocation([X,Y22]))));true),
    ((retract(frontier([X,Y22], P4, -1)), incr(P4, P42), assert(frontier([X,Y22], P42, -1)));true),
    ((retract(frontier([X,Y22], PTL4, WTL4)), incr(PTL4, NewPTL4), ((\+foundWumpus, incr(WTL4, NewWTL4)); NewWTL4 is -1)); (NewPTL4 is 1, NewWTL4 is 1)),
    ((NewWTL4 > 1, assert(foundWumpus), assert(wumpusLocation([X,Y22])), assert(frontier([X,Y22], NewPTL4, NewWTL4))); assert(frontier([X,Y22], NewPTL4, NewWTL4)))
  );true).


%RUN AGENT MAIN FUNCTION
run_agent(Percepts,Action):-
  format('\n====================================================R1\n'),
  display_world,   
  

  %Where is the agent right now, and what should be its next move
  getLastAction(LastAction),    

  getCurrentPosition(CurrentPosition),   

  getCurrentDirection(CurrentDirection),   

  

  %have i been here before? if no, then mark as visited and asses threat
                           %if yes, do not mark as visited, and do not asses threat
  shouldAgentAsses(CurrentPosition, Percepts),   

  getNextMove(Action),   


  format('LAST ACTION:         ~w\n', LastAction),
  format('NEXT ACTION:         ~w\n', Action),
  format('CURRENT POSITION:    [~w,~w]\n', CurrentPosition),
  format('CURRENT DIRECTION:   ~w\n\n', CurrentDirection),
  ((foundWumpus, wumpusLocation(WL), format('WUMPUS FOUND: [~w,~w]\n', WL));true), 
  format('VISITED: '), printAllVisited,
  format('\n\nTHREATS:\n'), printAllFrontier,


  %Begin taking action

  updatePosition(Percepts, Action, CurrentPosition, CurrentDirection),

  % getCurrentPosition(CurrentPosition),
 



  % updatePosition(Percepts, LastAction, CurrentPosition, CurrentDirection),
  % getCurrentPosition(CurrentPosition), 
  % getCurrentDirection(CurrentDirection),

  % updateAction(Percepts, Action, CurrentPosition, CurrentDirection).



  format('\n====================================================R2\n\n').








