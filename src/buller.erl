%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Bulletin functions using wse 
%%% @end
%%% Created :  7 Apr 2022 by Tony Rogvall <tony@rogvall.se>

-module(buller).

-export([run/2]).
-export([start/0]).

-compile(export_all).

-export([command/1]).
-export([rect/1]).
-export([text/1]).
-export([clear/0]).
-export([width/0]).
-export([height/0]).
-export([code_change/0]).

-define(BULLER_SERV, buller_serv).

start() ->
    application:ensure_all_started(buller).

run(Ws, Where) ->
    io:format("buller: called\n"),
    buller_serv:run(?BULLER_SERV, Ws, Where).

command(Commands) ->
    call(cmd, normalize(Commands)).

rect(Args) ->
    call(rect, args(Args)).

text(Args) ->
    call(text, args(Args)).

clear() ->
    call(clear, args([])).

width() ->
    call(width, args([])).

height() ->
    call(height, args([])).

code_change() ->
    call(code_change, args([])).

normalize(Cmd) when is_atom(Cmd) -> {Cmd,[]};
normalize({Cmd,Arg}) when is_atom(Cmd), not is_list(Arg) -> {Cmd,[Arg]};
normalize({Cmd,Args}) when is_atom(Cmd), not is_list(Args) -> {Cmd,Args};
normalize(Commands) when is_list(Commands) ->
    [normalize(Command) || Command <- Commands].

args(List) when is_list(List) ->
    maps:from_list(
      [if is_atom(Key) -> {Key,Value};
	  is_list(Key) -> 
	       {binary_to_atom(iolist_to_binary(Key)), Value}
       end || {Key,Value} <- List]);
args(Map) when is_map(Map) ->
    Map.

call(Req, Args) ->
    Ref = make_ref(),
    ?BULLER_SERV ! {Req, {self(),Ref}, Args},
    receive
	{Ref, Reply} ->
	    Reply
    end.
