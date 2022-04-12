%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Bulletin functions using wse 
%%% @end
%%% Created :  7 Apr 2022 by Tony Rogvall <tony@rogvall.se>

-module(buller).

-export([run/3]).
-export([start/0]).

-compile(export_all).

%% canvase
-export([draw/1]).
-export([draw_rect/1]).
-export([draw_text/1]).
-export([draw_image/1]).
-export([draw_video/1]).
-export([draw_pixel/1]).
-export([get_pixel/1]).
%% svg
-export([rect/1]).
-export([circle/1]).
-export([ellipse/1]).
-export([line/1]).
-export([polygon/1]).
-export([polyline/1]).
-export([path/1]).
-export([text/1]).
-export([set/1]).
-export([remove/1]).

%% get
-export([clear/0]).
-export([width/0]).
-export([height/0]).
-export([code_change/0]).

-define(BULLER_SERV, buller_serv).

start() ->
    application:ensure_all_started(buller).

run(Ws, CanvasID, SvgID) ->
    io:format("buller: called\n"),
    buller_serv:run(?BULLER_SERV, Ws, CanvasID, SvgID).

draw(Commands) ->
    call(draw, normalize(Commands)).

draw_rect(Args) ->
    call(draw_rect, args(Args)).

draw_text(Args) ->
    call(draw_text, args(Args)).

draw_image(Args) ->
    call(draw_image, args(Args)).

draw_video(Args) ->
    call(draw_video, args(Args)).

draw_pixel(Args) ->
    call(draw_pixel, args(Args)).

%% SVG
rect(Args) ->
    call(rect, args(Args)).

circle(Args) ->
    call(circle, args(Args)).

ellipse(Args) ->
    call(ellipse, args(Args)).

line(Args) ->
    call(line, args(Args)).

polygon(Args) ->
    call(polygon, args(Args)).

polyline(Args) ->
    call(polyline, args(Args)).

path(Args) ->
    call(path, args(Args)).

text(Args) ->
    call(text, args(Args)).

set(Args) ->
    call(set, args(Args)).

remove(Args) ->
    call(remove, args(Args)).

get_pixel(Args) ->
    call(get_pixel, args(Args)).

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
	  is_binary(Key) -> {binary_to_atom(Key), arg_value(Value)};
	  is_list(Key) -> 
	       {binary_to_atom(iolist_to_binary(Key)), arg_value(Value)}
       end || {Key,Value} <- List]);
args(Map) when is_map(Map) ->
    Map.

arg_value(Bin) when is_binary(Bin) -> binary_to_list(Bin);
arg_value(X) -> X.

call(Req, Args) ->
    Ref = make_ref(),
    ?BULLER_SERV ! {Req, {self(),Ref}, Args},
    receive
	{Ref, Reply} ->
	    Reply
    end.
