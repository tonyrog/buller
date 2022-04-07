%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Bulletin functions using wse 
%%% @end
%%% Created :  7 Apr 2022 by Tony Rogvall <tony@rogvall.se>

-module(buller_serv).

-export([run/3]).

-compile(export_all).

-record(s,
	{
	 ws,
	 canvas,
	 ctx,
	 width,
	 height
	}).

run(Server, Ws, Where) ->
    io:format("buller: run as ~p\n", [Server]),
    io:format("ID = ~p\n", [erlang:process_info(self(), registered_name)]),
    true = register(Server, self()),
    Canvas = wse:id(Where),
    {ok,Ctx} = wse:call(Ws, Canvas, getContext, ["2d"]),
    {ok,Width} = wse:get(Ws, Canvas, width),
    io:format("width of ~p = ~w\n", [Where, Width]),
    {ok,Height} = wse:get(Ws, Canvas, height),
    io:format("height of ~p = ~w\n", [Where, Height]),
    S = #s{ws=Ws, canvas=Canvas, ctx=Ctx, width=Width, height=Height},
    io:format("S = ~p\n", [S]),
    command_loop(S).

command_loop(S) ->
    receive
	{cmd, From, CmdList} ->
	    Result = command(S, CmdList),
	    reply(From, Result);
	{rect, From, Arg} ->
	    X = maps:get(x, Arg, 0),
	    Y = maps:get(y, Arg, 0),
	    Width = maps:get(width, Arg, 0),
	    Height = maps:get(height, Arg, 0),
	    Color = maps:get(color, Arg, "black"),
	    Result = command(S, 
			     [
			      {fillStyle, [Color]},
			      {fillRect,[X,Y,Width,Height]}
			     ]),
	    reply(From, Result),
	    command_loop(S);
	{text, From,  Arg} ->
	    X = maps:get(x, Arg, 0),
	    Y = maps:get(y, Arg, 0),
	    FontSize = maps:get('font-size', Arg, 24),
	    FontName = maps:get('font-name', Arg, "Arial"),
	    Font = case maps:get(font, Arg, undefined) of
		       undefined ->
			   integer_to_list(FontSize)++"px" ++
			       " "++FontName;
		       Fn -> Fn
		   end,
	    Text = maps:get(text, Arg, ""),
	    Color = maps:get(color, Arg, "black"),
	    Result = command(S,
			     [
			      {fillStyle,[Color]},
			      {font, [Font]},
			      {fillText,[Text,X,Y]}
			     ]),
	    reply(From, Result),
	    command_loop(S);
	{clear, From, Arg} ->
	    X = maps:get(x, Arg, 0),
	    Y = maps:get(y, Arg, 0),
	    Width = maps:get(width, Arg, S#s.width),
	    Height = maps:get(height, Arg, S#s.height),
	    Result = 
		command(S, [
			    {clearRect,[X,Y,Width,Height]}
			   ]),
	    reply(From, Result),
	    command_loop(S);

	{code_change, From, _Arg} ->
	    reply(From, ok),
	    ?MODULE:command_loop(S);
	    
	Other ->
	    io:format("got unknown command: ~p\n", [Other]),
	    command_loop(S)
    end.

command(S, [Command={Cmd,Args=[Arg]}|CmdList]) ->
    case property_type(Cmd, Args) of
	undefined ->
	    io:format("Command ~p\n", [Command]),
	    wse:call(S#s.ws, S#s.ctx, Cmd, Args),
	    command(S, CmdList);
	_Type ->    
	    io:format("Set ~p = ~p\n", [Cmd, Arg]),
	    wse:set(S#s.ws, S#s.ctx, Cmd, Arg),
	    command(S, CmdList)
    end;
command(S, [Command={Cmd,Args}|CmdList]) ->
    io:format("Command ~p\n", [Command]),
    wse:call(S#s.ws, S#s.ctx, Cmd, Args),
    command(S, CmdList);
command(_S, []) ->
    ok.

reply({Pid,Ref}, Reply) ->
    Pid ! {Ref, Reply}.

property_type(Cmd, [_Arg]) ->
    maps:get(Cmd, properties(), undefined);
property_type(_, _) -> %% only one argument
    undefined.

properties() ->
    #{ fillStyle => color,
       strokeStyle => color,
       shadowColor => color,
       shadowBlur => number,
       shadowOffsetX => number,
       shadowOffsetY => number,
       lineCap => {enum, [butt,round,square]},
       lineJoin => {enum,[bevel,round,miter]},
       lineWidth => number,
       miterLimit => number,
       font => string,  %% CSS font property syntax!
       textAlign => {enum,[center,'end',left,right,start]},
       textBaseline => {enum,[alphabetic,top,hanging,middle,
			      ideographic,bottom]},
       width => integer,
       height => integer,
       data => object,
       globalAlpha => number,
       globalCompositeOperation => {enum,
				    ['source-over','source-atop','source-in',
				     'source-out','destination-over',
				     'destination-atop','destination-in',
				     'destination-out','lighter',copy,'xor']}
     }.



