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
  safeSpots/1,       %list of all the safe square                      (LIST)
  canMakeSafeMove/0, %set to false by default                          (BOOL)
  currentSquare/1,   %cordinates of the square the agent is current at (LIST)
  facing/1,          %which direction the agent is facing (n,e,s,w)    (LIST)
  visited/1,         %list of all the squares the agent has visited    (LIST)
  actions/1,
  outOfBound/0,      %if the agent is out of bound                     (BOOL)
  frontier/3.        %keeps track of adjacent unvisited tiles and their danger levels


%RESET AGENT
clearAgent:-
  retractall(safeSpots(_)),
  retractall(canMakeSafeMove),
  retractall(currentSquare(_)),
  retractall(facing(_)),
  retractall(visited(_)),
  retractall(actions(_)),
  retractall(frontier(_,_,_)).

%INITIALIZE AGENT
init_agent:-
  format('\n====================================================I\n'),
  format('This is init_agent:\n\tIt gets called once, use it for your initialization\n\n'),


  clearAgent,

  asserta(actions(enterDungeon)),
  asserta(currentSquare([1,1])),
  asserta(safeSpots([1,1])),
  asserta(visited([1,1])),
  asserta(facing(east)),
  %asserta(frontier([2,1],0,0)), %cords, pit level, wumpus level
  %asserta(frontier([1,2],0,0)),

  assert(actions(turnleft)),
  assert(actions(goforward)),
  assert(actions(turnright)),
  assert(actions(turnright)),
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
  safeSpots(Spot), !.

getNextMove(Action):-
  retract(actions(A)), !,
  actions(Action), !.

printAllFrontier:-
  forall(frontier(C,B,W), format('~w  Pit%: ~w  Wump: ~w\n', [C,B,W])).

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
  Action = goforward, Bump=no, nextSquare(Position, Direction, NewPosition), asserta(currentSquare(NewPosition));
  Action = goforward, Bump=yes;
  Action = turnleft, lookLeft(Direction, NewDirection), asserta(facing(NewDirection));
  Action = turnright, lookRight(Direction, NewDirection), asserta(facing(NewDirection));
  Action = climb.

%if the tile has not been visited, mark it as visited
haveNotVisited(CurrentPos):-
  \+visited(CurrentPos),
  addVisited(CurrentPos),
  format('[~w,~w] Not Visited\n', CurrentPos).

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

assessThreat([no, no, _, _, _], _). %no threat (no breeze or scream)
assessThreat([no, yes, _, _, _], _).
%assessThreat([_, _, yes, _, _], _). %glitter
%assessThreat([_, _, _, yes, _], _). %bump
%assessThreat([_, _, _, _, yes], CurrentPos):- %scream 
  %replace every threatLevel list element to have a wumpus threat level of 0%
  
assessThreat([yes, no, _, _, _], CurrentPos):- %Stench
  [X,Y] = CurrentPos,
  %WTL stands for Wumpus Threat Level
  %PTL stands for Pit Threat Level
  %update the threat levels for the square to the right
  incr(X,X2),
  ((\+visited([X2,Y]),
  (
    ((retract(frontier([X2,Y], _, WTL1)), writeln(WTL1), incr(WTL1, NewWTL1)); NewWTL1 is 1), 
    assert(frontier([X2,Y], -1, NewWTL1))
  ));true),

  %update the threat levels for the square above
  incr(Y,Y22), 
  ((\+visited([X,Y22]),
  (
    ((retract(frontier([X,Y22], _, WTL2)), incr(WTL2, NewWTL2)); NewWTL2 is 1), 
    assert(frontier([X,Y22], -1, NewWTL2))
  ));true),

  %update the threat levels for the square to the left
  decr(X,X22),
  ((\+visited([X22,Y]),
  (
    ((retract(frontier([X22,Y], _, WTL3)), incr(WTL3, NewWTL3)); NewWTL3 is 1), 
    assert(frontier([X22,Y], -1, NewWTL3))
  ));true),

  %update the threat levels for the square below
  decr(Y,Y23),
  ((\+visited([X,Y23]),
  (
    ((retract(frontier([X,Y23], _, WTL4)), incr(WTL4, NewWTL4)); NewWTL4 is 1), 
    assert(frontier([X,Y23], -1, NewWTL4))
  ));true).




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
  format('VISITED: '), printAllVisited,
  format('\nWUMPUS THREATS:\n'), printAllFrontier,


  %Begin taking action
  updatePosition(Percepts, Action, CurrentPosition, CurrentDirection),
  getCurrentPosition(CurrentPosition),
  



  % updatePosition(Percepts, LastAction, CurrentPosition, CurrentDirection),
  % getCurrentPosition(CurrentPosition), 
  % getCurrentDirection(CurrentDirection),

  % updateAction(Percepts, Action, CurrentPosition, CurrentDirection).



  format('\n====================================================R2\n\n').



