-module(toh).
-author("Anmol").
-export([start/1]).

%%===========API's==================================
start(N) when N =< 15, N >= 1,is_integer(N)->
	put("A",lists:seq(1,N)),
	put("B",[]),
	put("C",[]),
	put(step,0),
	put(startTime,os:timestamp()),
	io:format("Objective is to move discs from Tower-A to Tower-B using a helping Tower, Tower-C~n",[]),
	print(),
	transfer(N,"A","B","C"),
	io:format("Total Time Taken: ~p millisecs~n",[timer:now_diff(os:timestamp(),get(startTime))]);
start(N)->
	io:format("Number of discs: ~p is not valid, should be an integer from 0 to 15 only~n",[N]).


%% ==========INTERNAL-FUNCTIONS=====================
transfer(0, _FromTower, _ToTower, _HelpTower) ->
	done;
transfer(N, FromTower, ToTower, HelpTower)->
	transfer(N-1, FromTower, HelpTower, ToTower),
	putDisc(FromTower, ToTower),
	transfer(N-1, HelpTower, ToTower, FromTower).

putDisc(FromTower, ToTower)->
	Step = get(step),
	put(step,Step+1),
	[Disc|Rest] = get(FromTower),
	put(FromTower,Rest),
	Discs = get(ToTower),
	put(ToTower,[Disc] ++ Discs),	
	io:format("STEP-~p~n",[get(step)]),
	io:format("put Disc from ~p to ~p~n",[FromTower,ToTower]),
	print().

print()->
	io:format("From-Tower-A:     ~p~n",[get("A")]),
	io:format("To-Tower-B:       ~p~n",[get("B")]),
	io:format("Helping-Tower-C:  ~p~n",[get("C")]),
	io:format("=================~n").
