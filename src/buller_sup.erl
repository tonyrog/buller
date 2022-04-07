-module(buller_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%%
%% Exported: start_link
%%

start_link() ->
    supervisor:start_link(?MODULE, []).

%%
%% Exported: init
%%

init([]) ->
    Spec =
	[#{id => buller_ibock,
	   start => {buller_ibock, start_link, [[]]}}],
    {ok, {#{strategy => one_for_all}, Spec}}.
