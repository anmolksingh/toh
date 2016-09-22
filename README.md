# Tower Of Hanoi

## Problem Statement
Tower Of Hanoi is a well known algorithm and it goes like this:<br/>
There is a stack of discs on a Source tower arranged such that smaller disc is placed on the bigger disc.
There is another Destination tower which has no discs initially, objective is to move all the discs from source tower to destination tower.
While transferring the discs from source tower to destination tower you are allowed to use one temporary tower as a helping tower.
However, the only **restriction** is that at any point in time on any tower bigger disc should not be placed on the smaller disc.

## About the Solution
- Solution is implemented as a standalone erlang module.
- Solution makes use of the process dictionary to store below information:
    a. Discs on three different towers.
    b. Number of discs movement.
    c. Start-time
- Single API, start(NumberOfDisc) is exposed.
- NumberOfDisc is checked to be of type integer and should not be greater than 15 and less than 0.

## How to Run the solution
Solution is implemented as a stand alone erlang module.<br/>
Below packages must be installed on the system:
```
- erlang
- git
```

**Step-1:**
```
git clone https://github.com/anmolksingh/toh.git
```

**Step-2:**
```
cd toh
```

**Step-3:**
```
 erlc toh.erl
```
This will create a toh.beam file in the same directory

**Step-4:**
Open an erlang shell using command erl
```
erl
```

**step-5:**
Inside the erlang shell, Call the start api with N which is the number of discs.
```
 toh:start(3).
```
*NOTE:* Only integer values from 0 to 15 are allowed as number of discs.

 
## OUTPUT
Below is the sample output
 
```
 Eshell V7.3  (abort with ^G)
 1> toh:start(3).
 Objective is to move discs from Tower-A to Tower-B using a helping Tower, Tower-C
 From-Tower-A:     [1,2,3]
 To-Tower-B:       []
 Helping-Tower-C:  []
 =================
 STEP-1
 put Disc from "A" to "B"
 From-Tower-A:     [2,3]
 To-Tower-B:       [1]
 Helping-Tower-C:  []
 =================
 STEP-2
 put Disc from "A" to "C"
 From-Tower-A:     [3]
 To-Tower-B:       [1]
 Helping-Tower-C:  [2]
 =================
 STEP-3
 put Disc from "B" to "C"
 From-Tower-A:     [3]
 To-Tower-B:       []
 Helping-Tower-C:  [1,2]
 =================
 STEP-4
 put Disc from "A" to "B"
 From-Tower-A:     []
 To-Tower-B:       [3]
 Helping-Tower-C:  [1,2]
 =================
 STEP-5
 put Disc from "C" to "A"
 From-Tower-A:     [1]
 To-Tower-B:       [3]
 Helping-Tower-C:  [2]
 =================
 STEP-6
 put Disc from "C" to "B"
 From-Tower-A:     [1]
 To-Tower-B:       [2,3]
 Helping-Tower-C:  []
 =================
 STEP-7
 put Disc from "A" to "B"
 From-Tower-A:     []
 To-Tower-B:       [1,2,3]
 Helping-Tower-C:  []
 =================
 Total Time Taken: 7971 millisecs
```

## Source Code
Please find the source code below:
```
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
```
