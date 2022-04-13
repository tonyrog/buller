
-type opt_value() :: number() | string().
-type arg_value() :: number() | string().
-type arg() :: {arg,Arg::arg_value()} |
	       {arg,Name::string(),Value::arg_value()}.
-type opt() :: {option,Name::string()} |
	       {option,Name::string(),Value::opt_value()}.
-type argl() :: {argl,Arg::arg_value(),
		 [{Name::string(),Value::arg_value()}]}.
-type option() :: arg()|opt()|argl().

%% combine {arg,Foo},{arg,K1,V1} ... {arg,Kn,Vn},
%%       into {argl,Foo,[{K1,V1}... {Kn,Vn}]}
-spec group_args(Options::[option()]) -> Options1::[option()].

-compile({nowarn_unused_function, {group_args,1}}).

group_args(Options) ->
    group_args_(Options,[]).

group_args_([{arg,Name}|Options], Acc) ->
    {Args, Options1} = collect_args_(Options,[]),
    group_args_(Options1, [{argl,Name,Args}|Acc]);
group_args_([Other|Options], Acc) ->
    group_args_(Options, [Other|Acc]);
group_args_([], Acc) ->
    lists:reverse(Acc).

collect_args_([{arg,Name,Value}|Options], Args) ->
    collect_args_(Options, [{Name,Value}|Args]);
collect_args_(Options, Args) ->
    {lists:reverse(Args), Options}.

-spec getopt(Name::string(), Options::[option()]) ->
	  undefined | Value::opt_value().

-compile({nowarn_unused_function, {getopt,2}}).
getopt(Name, Options) ->
    getopt(Name, Options, undefined).

-compile({nowarn_unused_function, {getopt,3}}).
getopt(Name, Options, Default) ->
    case getopt_(Name, Options) of
	{ok,Value} -> Value;
	undefined -> Default
    end.

-spec getbool(Name::string(), Options::[option()]) -> boolean().

-compile({nowarn_unused_function, {getbool,2}}).
getbool(Name, Options) ->
    case getopt_(Name, Options) of
	undefined -> false;
	{ok,false} -> false;
	{ok,true} -> true
    end.

getopt_(Name, [{option,Name}|_]) -> {ok,true};
getopt_(Name, [{option,Name,Value}|_]) -> {ok,Value};
getopt_(Name, [_|Opts]) -> getopt_(Name, Opts);
getopt_(_Name, []) -> undefined.

-spec parse_opts(Args::[string()]) -> [option()].
-compile({nowarn_unused_function, {parse_opts,1}}).

parse_opts([]) ->
    [];
parse_opts(Args) ->
    parse_opts_(Args,[]).

parse_opts_(Args,Acc) ->
    case parse_opt(Args) of
	{A,[]} -> lists:reverse([A|Acc]);
	{A,Args1} -> parse_opts_(Args1, [A|Acc])
    end.

parse_opt([[$-,$-|Opt] | Args]) ->
    parse_arg(option, Opt, Args);
parse_opt([Arg=[$-|Opt=[C|_]] | Args]) when ?is_digit(C) ->
    try to_integer_(Arg, 0) of
	Int -> {{arg,Int},Args}
    catch
	error:_ -> parse_arg(option,Opt,Args)
    end;
parse_opt([[$-|Opt] | Args]) ->
    parse_arg(option, Opt, Args);
parse_opt([Arg|Args]) ->
    try to_integer_(Arg, 0) of
	Int -> {{arg,Int},Args}
    catch
	error:_ -> 
	    parse_arg(arg,Arg,Args)
    end.

parse_arg(Tag, Name, Args) ->
    case string:split(Name, "=") of
	[Name1] ->
	    {{Tag,Name1},Args};
	[Name1,[]] ->
	    case Args of
		[] -> {{Tag,Name1,""},Args};
		[Arg=[$-,C|_]|Args1] when ?is_digit(C) ->
		    try to_number_(Arg) of
			Num -> {{Tag,Name1,Num},Args1}
		    catch
			error:_ -> {{Tag,Name1},Args}
		    end;
		[[$-|_]|_] -> {{Tag,Name1},Args};
		[Arg=[C|_]|Args1] when ?is_digit(C)->
		    try to_number_(Arg) of
			Num -> {{Tag,Name1,Num},Args1}
		    catch
			error:_ -> {{Tag,Name1,Arg},Args1}
		    end;
		[Arg|Args1] ->
		    {{Tag,Name1,Arg},Args1}
	    end;
	[Name1,Arg] ->
	    try to_number_(Arg) of
		Num -> {{Tag,Name1,Num},Args}
	    catch
		error:_ -> {{Tag,Name1,Arg},Args}
	    end
    end.

to_number_(Value) ->
    try to_integer_(Value,0) of
	Int -> Int
    catch
	error:_ ->
	    list_to_float(Value)
    end.

to_integer_("0x"++Value,0) -> list_to_integer(Value, 16);
to_integer_("-0x"++Value,0) -> -list_to_integer(Value, 16);
to_integer_(Value,0) -> list_to_integer(Value, 10);
to_integer_(Value,Base) -> list_to_integer(Value, Base).
