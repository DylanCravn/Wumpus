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
  oobTiles/1,
  actions/1,
  outOfBound/0,      %if the agent is out of bound                     (BOOL)
  frontier/3.        %keeps track of adjacent unvisited tiles and their danger levels


%RESET AGENT
clearAgent:-
  retractall(safeTiles(_)),
  retractall(oobTiles(_)),
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

  
  asserta(currentSquare([1,1])),
  asserta(safeTiles([1,1])),
  %asserta(visited([1,1])),
  asserta(facing(east)),
  %asserta(frontier([2,1],0,0)), %cords, pit level, wumpus level
  %asserta(frontier([1,2],0,0)),


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

getNextMove(Action, Action2):-
  (Action == enterDungeon, Action2 = turnright);
  (Action == turnright, Action2 = climb);true.


removeLastAction:-
  retract(actions(A)), !.

printAllFrontier:-  
  forall((frontier(D,-1,-1)), format('[~w,~w]  Safe\n', D)),
  forall((frontier(D,B,W), (B+W =\= -2)), format('~w  Pit: ~w  Wump: ~w\n', [D,B,W])).

printAllThreats:-
  forall((frontier(D,B,W), \+visited(D), \+safeTiles(D)), format('~w  Pit: ~w  Wump: ~w\n', [D,B,W])).

printAllVisited:-
  forall(visited(V), format('[~w,~w] ', V)).

printAllSafe:-
  forall(safeTiles(S), format('[~w,~w] ', S)).

printAllOob:-
  forall(oobTiles(B), format('[~w,~w] ', B)).

addSafe(S):-
  (\+safeTiles(S),
  assert(safeTiles(S)));true.

addVisited(V):-
  (\+visited(V),
  assert(visited(V)));true.

addOobTile(B):-
  (\+oobTiles(B),
  assert(oobTiles(B)), 
  (frontier(B,_,_), retract(frontier(B,_,_)))
  );true.




movedToNewTile(Action, Position):-
  Action = goforward,
  \+visited(Position).





%OUT OF BOUNDS 
markOutOfBoundsTile(Pos, east):-
  [X,Y] = Pos, incr(X,X1), addOobTile([X1,Y]).

markOutOfBoundsTile(Pos, north):-
  [X,Y] = Pos, incr(Y,Y1), addOobTile([X,Y1]).

markOutOfBoundsTile(Pos, west):-
  [X,Y] = Pos, decr(X,X1), addOobTile([X1,Y]).

markOutOfBoundsTile(Pos, south):-
  [X,Y] = Pos, decr(Y,Y1), addOobTile([X,Y1]).
  



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
  format('BUMP? ~w\n',Bump),
  format('Action? ~w\n',Action),
  Action = goforward, Bump=no, nextSquare(Position, Direction, NewPosition), asserta(currentSquare(NewPosition)), updateCurrentTileThreat(NewPosition);
  Bump=yes, format('BUMP!\n'), bumpReverse(Percept, Position, Direction), updatePosition([no,no,no,no,no], Action, Position, Direction);
  Action = turnleft, lookLeft(Direction, NewDirection), asserta(facing(NewDirection));
  Action = turnright, lookRight(Direction, NewDirection), asserta(facing(NewDirection));
  Action = climb;
  Action = grab;
  Action = enterDungeon.

updateAllFakeWumpus:-
  forall((frontier([X,Y],B,W), (W =\= -1, W<2)), (retract(frontier([X,Y],B,W)), assert(frontier([X,Y],B,-1))));true.

updateSafeTiles:-
  forall(frontier([X,Y],-1,-1), addSafe([X,Y])).

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
  updateFrontier(CurrentPos).

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

wasThatWumpus(Cords):-
  \+foundWumpus,
  [X,Y] = Cords,
  frontier([X,Y],_,W),
  W > 1,
  assert(foundWumpus),
  assert(wumpusLocation([X,Y])),
  retract(frontier([X,Y],_,W)),
  assert(frontier([X,Y],-1,W)).

noPit(Cords):-
  [X,Y] = Cords,
  frontier([X,Y],A,B),
  format('No Pit?...[~w,~w], ~w, ~w\n', [X,Y,A,B]),
  frontier([X,Y],-1,_), format(' True\n').

noWumpus(Cords):-
  [X,Y] = Cords,
  frontier([X,Y],A,B),
  format('No Wumpus?...[~w,~w], ~w, ~w\n', [X,Y,A,B]),
  frontier([X,Y],_,-1).

safeTile(Cords):- 
  [X,Y] = Cords,
  frontier([X,Y],-1,-1),
  format('Tile is safe\n').

isWumpusTile(Cords):-
  [X,Y] = Cords,
  (foundWumpus, frontier([X,Y],_,Wump), (Wump > 1)), %this is the wumpus tile, dont alter.
  format('Wumpus is on this tile\n').

updateOnlyW(Cords):- %no pit, increase W
  [X,Y] = Cords,
  (\+foundWumpus, 
  retract(frontier([X,Y], -1, W)), 
  incr(W, W2), 
  assert(frontier([X,Y], -1, W2))),
  format('No pit, W increased only\n'),
  wasThatWumpus([X,Y]); 
  true.

updateOnlyP(Cords):- %no wumpus increase P threat
  [X,Y] = Cords,
  retract(frontier([X,Y], P, -1)), 
  incr(P, P2), 
  assert(frontier([X,Y], P2, -1)),
  format('No Wumpus, increased only P\n').

updatePW(Cords):-
  [X,Y] = Cords,
  (retract(frontier([X,Y], PTL1, WTL1)), 
  incr(PTL1, NewPTL1), 
  ((\+foundWumpus, incr(WTL1, NewWTL1)); NewWTL1 is -1), 
  assert(frontier([X,Y], NewPTL1, NewWTL1)), 
  format('Updated Both P and W (~w,~w)', [PTL1, WTL1]), format('->(~w,~w)\n', [NewPTL1, NewWTL1]),
  (wasThatWumpus([X,Y]);true));
  true.


stenchBreezeUpdate(Cords):-
  [X,Y] = Cords,
  format('Updating [~w,~w]...\n', [X,Y]),
  (safeTile([X,Y]);
  isWumpusTile([X,Y]);
  (noPit([X,Y]), updateOnlyW([X,Y]));
  (noWumpus([X,Y]), updateOnlyP([X,Y]));
  updatePW([X,Y])),!.

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
  [X,Y] = CurrentPos, (([X,Y] == [2,3], format('Reached double square\n'));true),
  %((frontier([3,3],QQQ,WWW), format('In frontier: [~w,~w]\n', [QQQ,WWW]));true),
  %WTL stands for Wumpus Threat Level
  %PTL stands for Pit Threat Level

  %update the threat levels for the square to the right
  incr(X,Right),
  (((\+visited([Right,Y]), \+safeTiles([Right,Y])), stenchBreezeUpdate([Right,Y]));true), 
  format('Passed Right Tile\n'),

  %update the threat levels for the square above
  incr(Y,Up), 
  (((\+visited([X,Up]), \+safeTiles([X,Up])), stenchBreezeUpdate([X,Up]));true),
  format('Passed Up Tile\n'),

  %update the threat levels for the square to the left
  decr(X,Left),
  (((\+visited([Left,Y]), \+safeTiles([Left,Y])), stenchBreezeUpdate([Left,Y]));true),
  format('Passed Left Tile\n'),
 
  % %sqaure below
  decr(Y,Down),
  (((\+visited([X,Down]), \+safeTiles([X,Down])), stenchBreezeUpdate([X,Down]));true),
  format('Passed Down Tile\n'),

  ((foundWumpus, updateAllFakeWumpus);true).


bumpReverse(Percept, Pos, D):-

  retract(currentSquare(Pos)),
  getCurrentPosition(P2),
  markOutOfBoundsTile(P2, D),
  
  retract(visited(Pos)),
  retract(safeTiles(Pos)); true.


%RUN AGENT MAIN FUNCTION
run_agent(Percepts,Action):-
  format('\n====================================================R1\n'),
  display_world,   

  %Where is the agent right now, and what should be its next move
    
  getCurrentPosition(CurrentPosition),   
  getCurrentDirection(CurrentDirection),  

  %have i been here before? if no, then mark as visited and asses threat
                           %if yes, do not mark as visited, and do not asses threat
  shouldAgentAsses(CurrentPosition, Percepts),
  updateSafeTiles,  

  getNextMove(Action),   

  format('NEXT ACTION:         ~w\n', Action),
  format('CURRENT POSITION:    [~w,~w]\n', CurrentPosition),
  format('CURRENT DIRECTION:   ~w\n\n', CurrentDirection),
  ((foundWumpus, wumpusLocation(WL), format('WUMPUS FOUND: [~w,~w]\n', WL));true), 
  format('VISITED:  '), printAllVisited,
  format('\nSAFE:     '), printAllSafe,
  format('\nOOB:    '), printAllOob,
  format('\n\nTHREATS:\n'), printAllThreats,

  %Begin taking action
  updatePosition(Percepts, Action, CurrentPosition, CurrentDirection),
  removeLastAction,

  format('\n====================================================R2\n\n').












