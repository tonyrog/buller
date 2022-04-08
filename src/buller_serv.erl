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
	 screen_width,
	 screen_height,
	 screen_avail_width,
	 screen_avail_height,
	 screen_colorDepth,
	 screen_pixelDepth,
	 window_width,
	 window_height,
	 canvas_width,
	 canvas_height
	}).

-define(rec_info(T,R),lists:zip(record_info(fields,T),tl(tuple_to_list(R)))).


run(Server, Ws, Where) ->
    true = register(Server, self()),
    Canvas = wse:id(Where),
    {ok,Ctx} = wse:call(Ws, Canvas, getContext, ["2d"]),
    {ok,CanvasWidth} = wse:get(Ws, Canvas, width),
    {ok,CanvasHeight} = wse:get(Ws, Canvas, height),

    {ok,ScreenWidth} = wse:get(Ws, wse:id(screen), width),
    {ok,ScreenHeight} = wse:get(Ws, wse:id(screen), height),

    {ok,ScreenAvailWidth} = wse:get(Ws, wse:id(screen), availWidth),
    {ok,ScreenAvailHeight} = wse:get(Ws, wse:id(screen), availHeight),

    {ok,WindowWidth} = wse:get(Ws, wse:id(window), innerWidth),
    {ok,WindowHeight} = wse:get(Ws, wse:id(window), innerHeight),

    {ok,ScreenColorDepth} = wse:get(Ws, wse:id(screen), colorDepth),
    {ok,ScreenPixelDepth} = wse:get(Ws, wse:id(screen), pixelDepth),

    S = #s{ws=Ws, 
	   canvas=Canvas, 
	   ctx=Ctx,
	   canvas_width=CanvasWidth, 
	   canvas_height=CanvasHeight,
	   window_width=WindowWidth, 
	   window_height=WindowHeight,
	   screen_width=ScreenWidth, 
	   screen_height=ScreenHeight,
	   screen_avail_width=ScreenAvailWidth, 
	   screen_avail_height=ScreenAvailHeight,
	   screen_colorDepth=ScreenColorDepth,
	   screen_pixelDepth=ScreenPixelDepth
	  },
    io:format("S = ~p\n", [?rec_info(s,S)]),
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
	    Width = maps:get(width, Arg, S#s.canvas_width),
	    Height = maps:get(height, Arg, S#s.canvas_height),
	    Result = 
		command(S, [
			    {clearRect,[X,Y,Width,Height]}
			   ]),
	    reply(From, Result),
	    command_loop(S);

	{image, From, Arg} ->
	    X = maps:get(x, Arg, undefined),
	    Y = maps:get(y, Arg, undefined),
	    if X =:= undefined; Y =:= undefined ->
		    reply(From, {error, missing_pos}),
		    command_loop(S);
	       true ->
		    case maps:get(src, Arg, undefined) of
			undefined ->
			    reply(From, {error, missing_src}),
			    command_loop(S);
			Src ->
			    case wse:load_image(S#s.ws, Src) of
				{ok, Img} ->
				    Result = image_command(S, Img,
							   X, Y, Arg),
				    reply(From, Result),
				    command_loop(S);
				_Error ->
				    reply(From, {error, image_error}),
				    command_loop(S)
			    end
		    end
	    end;

	{video, From, Arg} ->
	    X = maps:get(x, Arg, undefined),
	    Y = maps:get(y, Arg, undefined),
	    Width = maps:get(width, Arg, undefined),
	    Height = maps:get(height, Arg, undefined),
	    if X =:= undefined; Y =:= undefined ->
		    reply(From, {error, missing_pos}),
		    command_loop(S);
	       true ->
		    case maps:get(src, Arg, undefined) of
			undefined ->
			    reply(From, {error, missing_src}),
			    command_loop(S);
			Src ->
			    case wse:load_video(S#s.ws, Src, Width, Height) of
				{ok, Vid} ->
				    Result = image_command(S, Vid,
							   X, Y, Arg),
				    reply(From, Result),
				    command_loop(S);
				_Error ->
				    reply(From, {error, image_error}),
				    command_loop(S)
			    end
		    end
	    end;

	{setpixel, From, Arg} ->
	    X = maps:get(x, Arg, 0),
	    Y = maps:get(y, Arg, 0),
	    C = maps:get(color,Arg,"#000000"),
	    case lookup_rgba(C) of
		false -> 
		    reply(From, {error, color_error}),
		    command_loop(S);
		{R,G,B,A} ->
		    %% io:format("R=~w, G=~w, B=~w, A=~w\n", [R,G,B,A]),
		    {ok,ImageData} = wse:call(S#s.ws, S#s.ctx, getImageData,
					      [X,Y,1,1]),
		    {ok,Data} = wse:get(S#s.ws, ImageData, data),
		    wse:set(S#s.ws, Data, 0, R),
		    wse:set(S#s.ws, Data, 1, G),
		    wse:set(S#s.ws, Data, 2, B),
		    wse:set(S#s.ws, Data, 3, A),
		    wse:call(S#s.ws, S#s.ctx, putImageData, [ImageData,X,Y]),
		    reply(From, ok),
		    command_loop(S)
	    end;

	{getpixel, From, Arg} ->
	    X = maps:get(x, Arg, 0),
	    Y = maps:get(y, Arg, 0),	    
	    {ok,ImageData} = wse:call(S#s.ws, S#s.ctx, getImageData, [X,Y,1,1]),
	    {ok,Data} = wse:get(S#s.ws, ImageData, data),
	    {ok,R} = wse:get(S#s.ws, Data, 0),
	    {ok,G} = wse:get(S#s.ws, Data, 1),
	    {ok,B} = wse:get(S#s.ws, Data, 2),
	    {ok,A} = wse:get(S#s.ws, Data, 3),
	    %% io:format("R=~w, G=~w, B=~w, A=~w\n", [R,G,B,A]),
	    Value = "#" ++ 
		tl(integer_to_list(16#100+R,16)) ++
		tl(integer_to_list(16#100+G,16)) ++
		tl(integer_to_list(16#100+B,16)) ++
		tl(integer_to_list(16#100+A,16)),
	    reply(From, Value),
	    command_loop(S);

	{width, From, _Arg} ->
	    reply(From, S#s.canvas_width),
	    command_loop(S);

	{height, From, _Arg} ->
	    reply(From, S#s.canvas_height),
	    command_loop(S);

	{code_change, From, _Arg} ->
	    reply(From, ok),
	    ?MODULE:command_loop(S);
	    
	Other ->
	    io:format("got unknown command: ~p\n", [Other]),
	    command_loop(S)
    end.

%% src, img, x, y
%% src, img, x, y, width, height
%% src, img, sx, sy, swidth, sheight, x, y, width, height

image_command(S = #s { ws = Ws }, Img, X, Y, Arg) ->
    {ok,IWidth} = wse:get(Ws, Img, width),
    {ok,IHeight} = wse:get(Ws, Img, height),
    
    Width = maps:get(width, Arg, undefined),
    Height = maps:get(height, Arg, undefined),
    
    Sx = maps:get(sx, Arg, undefied),
    Sy = maps:get(sy, Arg, undefined),
    Swidth = maps:get(swidth, Arg, undefined),
    Sheight = maps:get(sheight, Arg, undefined),

    if 
	Sx =/= undefined, Sy =/= undefined;
	Swidth =/= undefined, Sy =/= undefined ->  %% clip
	    command(S,
		    [
		     {drawImage,
		      [Img,X,Y,
		       ifundef(Width,IWidth),
		       ifundef(Height,IHeight),
		       ifundef(Sx, X),
		       ifundef(Sy, Y),
		       ifundef(Swidth, Width),
		       ifundef(Sheight, Height)
		      ]}
		    ]);
	Width =/= undefined, Height =/= undefined ->
	    command(S,
		    [
		     {drawImage,[Img,X,Y,Width,Height]}
		    ]);
	true ->
	    command(S,
		    [
		     {drawImage,[Img,X,Y]}
		    ])
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

ifundef(undefined, Y) -> Y;
ifundef(X, _Y) ->  X.

    

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

-define(rgba(R,G,B,A), {R,G,B,A}).
-define(rgb(R,G,B), {R,G,B,255}).

lookup_rgba([$#,R1,R2,G1,G2,B1,B2]) ->
    R = list_to_integer([R1,R2],16),
    G = list_to_integer([G1,G2],16),
    B = list_to_integer([B1,B2],16),
    ?rgb(R,G,B);
lookup_rgba([$#,R1,R2,G1,G2,B1,B2,A1,A2]) ->
    R = list_to_integer([R1,R2],16),
    G = list_to_integer([G1,G2],16),
    B = list_to_integer([B1,B2],16),
    A = list_to_integer([A1,A2],16),
    ?rgba(R,G,B,A);
lookup_rgba([$#|Hs]) ->
    C = list_to_integer(Hs,16),
    R = (C bsr 16) band 16#ff,
    G = (C bsr 8) band 16#ff,
    B = C band 16#ff,
    ?rgb(R, G, B);
lookup_rgba(Name) when is_atom(Name) -> color_from_name(Name);
lookup_rgba(Name) when is_list(Name) -> color_from_name(Name).

color_from_name(Name) when is_atom(Name) ->
    color_from_name(atom_to_list(Name));
color_from_name(Name) when is_list(Name) ->
    case string:to_lower(Name) of
	"aliceblue"    -> ?rgb(16#F0,16#F8,16#FF);
	"antiquewhite" -> ?rgb(16#FA,16#EB,16#D7);
	"aqua"         -> ?rgb(16#00,16#FF,16#FF);
	"aquamarine"   -> ?rgb(16#7F,16#FF,16#D4);
	"azure"        -> ?rgb(16#F0,16#FF,16#FF);
	"beige"        -> ?rgb(16#F5,16#F5,16#DC);
	"bisque"       -> ?rgb(16#FF,16#E4,16#C4);
	"black"        -> ?rgb(16#00,16#00,16#00);
	"blanchedalmond" ->	?rgb(16#FF,16#EB,16#CD);
	"blue" ->	  ?rgb(16#00,16#00,16#FF);
	"blueviolet" ->	?rgb(16#8A,16#2B,16#E2);
	"brown" ->	        ?rgb(16#A5,16#2A,16#2A);
	"burlywood" ->	?rgb(16#DE,16#B8,16#87);
	"cadetblue" ->	?rgb(16#5F,16#9E,16#A0);
	"chartreuse" ->	?rgb(16#7F,16#FF,16#00);
	"chocolate" ->	?rgb(16#D2,16#69,16#1E);
	"coral" ->	?rgb(16#FF,16#7F,16#50);
	"cornflowerblue" ->	?rgb(16#64,16#95,16#ED);
	"cornsilk" ->	?rgb(16#FF,16#F8,16#DC);
	"crimson" ->	?rgb(16#DC,16#14,16#3C);
	"cyan" ->	?rgb(16#00,16#FF,16#FF);
	"darkblue" ->	?rgb(16#00,16#00,16#8B);
	"darkcyan" ->	?rgb(16#00,16#8B,16#8B);
	"darkgoldenrod" ->	?rgb(16#B8,16#86,16#0B);
	"darkgray" ->	?rgb(16#A9,16#A9,16#A9);
	"darkgrey" ->	?rgb(16#A9,16#A9,16#A9);
	"darkgreen" ->	?rgb(16#00,16#64,16#00);
	"darkkhaki" ->	?rgb(16#BD,16#B7,16#6B);
	"darkmagenta" ->	?rgb(16#8B,16#00,16#8B);
	"darkolivegreen" ->	?rgb(16#55,16#6B,16#2F);
	"darkorange" ->	?rgb(16#FF,16#8C,16#00);
	"darkorchid" ->	?rgb(16#99,16#32,16#CC);
	"darkred" ->	?rgb(16#8B,16#00,16#00);
	"darksalmon" ->	?rgb(16#E9,16#96,16#7A);
	"darkseagreen" ->	?rgb(16#8F,16#BC,16#8F);
	"darkslateblue" ->	?rgb(16#48,16#3D,16#8B);
	"darkslategray" ->	?rgb(16#2F,16#4F,16#4F);
	"darkslategrey" ->	?rgb(16#2F,16#4F,16#4F);
	"darkturquoise" ->	?rgb(16#00,16#CE,16#D1);
	"darkviolet" ->	?rgb(16#94,16#00,16#D3);
	"deeppink" ->	?rgb(16#FF,16#14,16#93);
	"deepskyblue" ->	?rgb(16#00,16#BF,16#FF);
	"dimgray" ->	?rgb(16#69,16#69,16#69);
	"dimgrey" ->	?rgb(16#69,16#69,16#69);
	"dodgerblue" ->	?rgb(16#1E,16#90,16#FF);
	"firebrick" ->	?rgb(16#B2,16#22,16#22);
	"floralwhite" ->	?rgb(16#FF,16#FA,16#F0);
	"forestgreen" ->	?rgb(16#22,16#8B,16#22);
	"fuchsia" ->	?rgb(16#FF,16#00,16#FF);
	"gainsboro" ->	?rgb(16#DC,16#DC,16#DC);
	"ghostwhite" ->	?rgb(16#F8,16#F8,16#FF);
	"gold" ->	?rgb(16#FF,16#D7,16#00);
	"goldenrod" ->	?rgb(16#DA,16#A5,16#20);
	"gray" ->	?rgb(16#80,16#80,16#80);
	"grey" ->	?rgb(16#80,16#80,16#80);
	"green" ->	?rgb(16#00,16#80,16#00);
	"greenyellow" ->	?rgb(16#AD,16#FF,16#2F);
	"honeydew" ->	?rgb(16#F0,16#FF,16#F0);
	"hotpink" ->	?rgb(16#FF,16#69,16#B4);
	"indianred" -> 	?rgb(16#CD,16#5C,16#5C);
	"indigo" -> 	?rgb(16#4B,16#00,16#82);
	"ivory" ->	?rgb(16#FF,16#FF,16#F0);
	"khaki" ->	?rgb(16#F0,16#E6,16#8C);
	"lavender" ->	?rgb(16#E6,16#E6,16#FA);
	"lavenderblush" ->	?rgb(16#FF,16#F0,16#F5);
	"lawngreen" ->	?rgb(16#7C,16#FC,16#00);
	"lemonchiffon" ->	?rgb(16#FF,16#FA,16#CD);
	"lightblue" ->	?rgb(16#AD,16#D8,16#E6);
	"lightcoral" ->	?rgb(16#F0,16#80,16#80);
	"lightcyan" ->	?rgb(16#E0,16#FF,16#FF);
	"lightgoldenrodyellow" ->	?rgb(16#FA,16#FA,16#D2);
	"lightgray" ->	?rgb(16#D3,16#D3,16#D3);
	"lightgrey" ->	?rgb(16#D3,16#D3,16#D3);
	"lightgreen" ->	?rgb(16#90,16#EE,16#90);
	"lightpink" ->	?rgb(16#FF,16#B6,16#C1);
	"lightsalmon" ->	?rgb(16#FF,16#A0,16#7A);
	"lightseagreen" ->	?rgb(16#20,16#B2,16#AA);
	"lightskyblue" ->	?rgb(16#87,16#CE,16#FA);
	"lightslategray" ->	?rgb(16#77,16#88,16#99);
	"lightslategrey" ->	?rgb(16#77,16#88,16#99);
	"lightsteelblue" ->	?rgb(16#B0,16#C4,16#DE);
	"lightyellow" ->	?rgb(16#FF,16#FF,16#E0);
	"lime" ->	?rgb(16#00,16#FF,16#00);
	"limegreen" ->	?rgb(16#32,16#CD,16#32);
	"linen" ->	?rgb(16#FA,16#F0,16#E6);
	"magenta" ->	?rgb(16#FF,16#00,16#FF);
	"maroon" ->	?rgb(16#80,16#00,16#00);
	"mediumaquamarine" ->	?rgb(16#66,16#CD,16#AA);
	"mediumblue" ->	?rgb(16#00,16#00,16#CD);
	"mediumorchid" ->	?rgb(16#BA,16#55,16#D3);
	"mediumpurple" ->	?rgb(16#93,16#70,16#D8);
	"mediumseagreen" ->	?rgb(16#3C,16#B3,16#71);
	"mediumslateblue" ->	?rgb(16#7B,16#68,16#EE);
	"mediumspringgreen" ->	?rgb(16#00,16#FA,16#9A);
	"mediumturquoise" ->	?rgb(16#48,16#D1,16#CC);
	"mediumvioletred" ->	?rgb(16#C7,16#15,16#85);
	"midnightblue" ->	?rgb(16#19,16#19,16#70);
	"mintcream" ->	?rgb(16#F5,16#FF,16#FA);
	"mistyrose" ->	?rgb(16#FF,16#E4,16#E1);
	"moccasin" ->	?rgb(16#FF,16#E4,16#B5);
	"navajowhite" ->	?rgb(16#FF,16#DE,16#AD);
	"navy" ->	?rgb(16#00,16#00,16#80);
	"oldlace" ->	?rgb(16#FD,16#F5,16#E6);
	"olive" ->	?rgb(16#80,16#80,16#00);
	"olivedrab" ->	?rgb(16#6B,16#8E,16#23);
	"orange" ->	?rgb(16#FF,16#A5,16#00);
	"orangered" ->	?rgb(16#FF,16#45,16#00);
	"orchid" ->	?rgb(16#DA,16#70,16#D6);
	"palegoldenrod" ->	?rgb(16#EE,16#E8,16#AA);
	"palegreen" ->	?rgb(16#98,16#FB,16#98);
	"paleturquoise" ->	?rgb(16#AF,16#EE,16#EE);
	"palevioletred" ->	?rgb(16#D8,16#70,16#93);
	"papayawhip" ->  ?rgb(16#FF,16#EF,16#D5);
	"peachpuff" ->	  ?rgb(16#FF,16#DA,16#B9);
	"peru" ->	  ?rgb(16#CD,16#85,16#3F);
	"pink" ->	  ?rgb(16#FF,16#C0,16#CB);
	"plum" ->	  ?rgb(16#DD,16#A0,16#DD);
	"powderblue" ->  ?rgb(16#B0,16#E0,16#E6);
	"purple" ->	  ?rgb(16#80,16#00,16#80);
	"red" ->	  ?rgb(16#FF,16#00,16#00);
	"rosybrown" ->	  ?rgb(16#BC,16#8F,16#8F);
	"royalblue" ->	  ?rgb(16#41,16#69,16#E1);
	"saddlebrown" -> ?rgb(16#8B,16#45,16#13);
	"salmon" ->	  ?rgb(16#FA,16#80,16#72);
	"sandybrown" ->  ?rgb(16#F4,16#A4,16#60);
	"seagreen" ->	  ?rgb(16#2E,16#8B,16#57);
	"seashell" ->	  ?rgb(16#FF,16#F5,16#EE);
	"sienna" ->	  ?rgb(16#A0,16#52,16#2D);
	"silver" ->	  ?rgb(16#C0,16#C0,16#C0);
	"skyblue" ->	  ?rgb(16#87,16#CE,16#EB);
	"slateblue" ->	  ?rgb(16#6A,16#5A,16#CD);
	"slategray" ->	  ?rgb(16#70,16#80,16#90);
	"slategrey" ->	  ?rgb(16#70,16#80,16#90);
	"snow" ->	          ?rgb(16#FF,16#FA,16#FA);
	"springgreen" ->       ?rgb(16#00,16#FF,16#7F);
	"steelblue" ->	  ?rgb(16#46,16#82,16#B4);
	"tan" ->	          ?rgb(16#D2,16#B4,16#8C);
	"teal" ->	          ?rgb(16#00,16#80,16#80);
	"thistle" ->	          ?rgb(16#D8,16#BF,16#D8);
	"tomato" ->	          ?rgb(16#FF,16#63,16#47);
	"turquoise" ->	  ?rgb(16#40,16#E0,16#D0);
	"violet" ->	          ?rgb(16#EE,16#82,16#EE);
	"wheat" ->	          ?rgb(16#F5,16#DE,16#B3);
	"white" ->	          ?rgb(16#FF,16#FF,16#FF);
	"whitesmoke" ->        ?rgb(16#F5,16#F5,16#F5);
	"yellow" ->	          ?rgb(16#FF,16#FF,16#00);
	"yellowgreen" ->       ?rgb(16#9A,16#CD,16#32);
	_ -> false
    end.
