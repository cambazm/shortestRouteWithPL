% BLG435E-AI Fall/2005 HW#1
% Student Number: 040020365
% Student Name: Mehmet CAMBAZ
%-------------------------------------------------------------------------
% This program finds the shortest route between two cities.

% Map of Romania (undirected graph):
% Numbers represent kilometers.
edge(arad,zerind,75).
edge(arad,timisoara,118).
edge(arad,sibiu,140).
edge(zerind,oradea,71).
edge(oradea,sibiu,151).
edge(timisoara,lugoj,111).
edge(lugoj,mehadia,70).
edge(mehadia,dobreta,75).
edge(dobreta,craiova,120).
edge(craiova,pitesti,138).
edge(sibiu,rimnicu_vilcea,80).
edge(sibiu,fagaras,99).
edge(rimnicu_vilcea,craiova,146).
edge(rimnicu_vilcea,pitesti,97).
edge(fagaras,bucharest,211).
edge(pitesti,bucharest,101).
edge(bucharest,giurgiu,90).
edge(bucharest,urziceni,85).
edge(urziceni,hirsova,98).
edge(hirsova,eforie,86).
edge(urziceni,vaslui,142).
edge(vaslui,iasi,92).
edge(iasi,neamt,87).
%-------------------------------------------------------------------------
find_shortestroute(Start,Finish,Route,Cost) :-
    assert(best([], 9999)),
    route([Start,Finish],Start,Finish,Route,Cost),
    compare(Route,Cost),
    fail.

find_shortestroute(Start,Finish,Route,Cost) :- best(Route,Cost). 

compare(Route,Cost) :-
   best(BestRoute, BestCost),
   change(Route,Cost,BestRoute, BestCost).

change(Route,Cost,BestRoute, BestCost) :- BestCost =< Cost, !.

change(Route,Cost,BestRoute, BestCost) :-
   retract(best(BestRoute,BestCost)),
   assert(best(Route,Cost)).

%-------------------------------------------------------------------------
route(Used,Start,Finish,[Start,Finish],Cost) :- edge(Start,Finish,Cost).

route(Used,Start,Finish,[Start,Finish],Cost) :- edge(Finish,Start,Cost).

% The followings are used to avoid visits to nodes which are already part of the route. 

route(Used,Start,Finish,[Start|Rest],Cost) :-
   edge(Start,Midpoint,Cost1),
   not(member(Midpoint,Used)),
   route([Midpoint|Used],Midpoint,Finish,Rest,Cost2),
   Cost is Cost1 + Cost2.

route(Used,Start,Finish,[Start|Rest],Cost) :-
   edge(Midpoint,Start,Cost1),
   not(member(Midpoint,Used)),
   route([Midpoint|Used],Midpoint,Finish,Rest,Cost2),
   Cost is Cost1 + Cost2.

%predicate that gives length of the list
list_length([],0).
list_length([_|T],N1) :-
	list_length(T,N),
	N1 is N+1.

basla :-
	nl,nl,
	write('Route Finding Program'),nl,
	write('1. Find the average branching factor for the map of Romania'),nl,
	write('2. Find the shortest route between two cities'),nl,
	write('3. Exit from program'),nl,nl,
	write('Enter your choice:'),nl,nl,
	read(X),										%get choice
	menu(X).										%do according to choice

menu(1) :-
	findall(SourceCity, edge(SourceCity,_,_), S),
	list_length(S,NumberOfPaths),					%there are paths according to edges predicates
	NumberofEdges is NumberOfPaths*2,				%edges are two wayed, so number of edges are 2 times paths
	findall(DestCity, edge(_,DestCity,_), D),
	list_to_set(S,SetS),							%set does not contain duplicates
	list_to_set(D,SetD),
	union(SetS,SetD,All),							%all cities with no duplicates
	list_length(All,NumberOfCities),				%number of cities
	ABF is NumberofEdges/NumberOfCities,			%ABF is Average brach factor
	write('Average branch factor is: '),
	write(ABF),nl,nl,
	basla.

menu(2) :-
	findall(SourceCity, edge(SourceCity,_,_), S),
	findall(DestCity, edge(_,DestCity,_), D),
	list_to_set(S,SetS),
	list_to_set(D,SetD),
	union(SetS,SetD,All),							%all cities with no duplicates
	nl,nl,
	write('Cities: '),nl,nl,
	write(All),nl,									%list all cities
	nl,nl,
	write('Start city: '),nl,
	read(Start),nl,
	write('End city: '),nl,
	read(Finish),nl,
	find_shortestroute(Start,Finish,Route,Cost),	%find the shortest route
	write('Shortest route is: '),
	write(Route),nl,
	write('Cost: '),
	write(Cost),nl,nl,
	basla.

menu(3) :-
	write('Goodbye'),nl,nl.

menu(X) :-
	write('Incorrect choice'),nl,nl,				%unrecognizable choice input
	basla.

:-basla.