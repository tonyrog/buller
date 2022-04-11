%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created :  7 Apr 2022 by Tony Rogvall <tony@rogvall.se>

-module(buller_ibock).

-export([start_link/1]).
-export([handle_http_request/4]).

-include_lib("rester/include/rester_http.hrl").

-define(log(F,W,As),
	io:format("~s:~w: " ++ (W)++" "++(F)++"\n", [?MODULE, ?LINE | (As)])).	
%% -define(debug(F,As), ?log(F,"debug",As)).
-define(debug(F,A), ok).
-define(info(F,As),  ?log(F,"info", As)).
-define(warn(F,As),  ?log(F,"warn", As)).
-define(error(F,As), ?log(F,"error", As)).

-define(DEFAULT_PORT, 1237).
-define(DEFAULT_IFADDR, any).

start_link(Args0) ->
    Args = Args0 ++ application:get_all_env(buller),    
    Port = proplists:get_value(port, Args, ?DEFAULT_PORT),
    IfAddr = proplists:get_value(ifaddr, Args, ?DEFAULT_IFADDR),
    ResterHttpArgs =
        [{request_handler,
          {?MODULE, handle_http_request, []}},
         {verify, verify_none},
         {ifaddr, IfAddr},
         %% {certfile, CertFilename},
         {nodelay, true},
         {reuseaddr, true}],
    rester_http_server:start_link(Port, ResterHttpArgs).


handle_http_request(Socket, Request, Body, XArgs) ->
    ?debug("handle_http_request: ~s ~s ~p\n",
	   [rester_http:format_request(Request),
	    rester_http:format_hdr(Request#http_request.headers),
	    Body]),
    try
        case Request#http_request.method of
            'POST' ->
                handle_http_response(
                  Socket, Request,
                  handle_http_post(Socket, Request, Body, tl(XArgs)));
            'GET' ->
                handle_http_response(
                  Socket, Request,
                  handle_http_get(Socket, Request, Body, tl(XArgs)));
            _ ->
                response(Socket, Request, 405, default_phrase(405))
        end
    catch
        _Class:Reason:StackTrace ->
	    ?error("handle_http_request: ~p\n", [StackTrace]),
	    erlang:error(Reason)
    end.

handle_http_post(_Socket, Request, Body, _XArgs) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
        ["v1","draw_text"] ->
	    Args = parse_json_body(Request, Body),
	    buller:draw_text(Args),
	    {200, "OK",
	     [{content_type, "text/plain"}]};
        ["v1","draw_rect"] ->
	    Args = parse_json_body(Request, Body),
	    buller:draw_rect(Args),
	    {200, "OK",
	     [{content_type, "text/plain"}]};
        ["v1","draw_image"] ->
	    Args = parse_json_body(Request, Body),
	    buller:draw_image(Args),
	    {200, "OK",
	     [{content_type, "text/plain"}]};
        ["v1","draw_video"] ->
	    Args = parse_json_body(Request, Body),
	    buller:draw_video(Args),
	    {200, "OK",
	     [{content_type, "text/plain"}]};
	["v1","draw_pixel"] ->
	    Args = parse_json_body(Request, Body),
	    buller:draw_pixel(Args),
	    {200, "OK",
	     [{content_type, "text/plain"}]};
	%% SVG commands
        ["v1","text"] ->
	    Args = parse_json_body(Request, Body),
	    buller:text(Args),
	    {200, "OK",
	     [{content_type, "text/plain"}]};
        ["v1","rect"] ->
	    Args = parse_json_body(Request, Body),
	    buller:rect(Args),
	    {200, "OK",
	     [{content_type, "text/plain"}]};
	%% general
        ["v1","clear"] ->
	    %% Args = parse_json_body(Request, Body),
	    buller:clear(),
	    {200, "OK",
	     [{content_type, "text/plain"}]};


        _Tokens ->
            501
    end.


handle_http_get(_Socket, #http_request{uri = Url}, _Body, _XArgs) ->
    case string:tokens(Url#url.path, "/") of
        ["v1","width"] ->
	    {200, integer_to_list(buller:width()),
	     [{content_type, "text/plain"}]};
        ["v1","height"] ->
	    {200, integer_to_list(buller:height()),
	     [{content_type, "text/plain"}]};
        ["v1","pixel"] ->
            Args = uri_string:dissect_query(Url#url.querypart),
	    Pixel = buller:getpixel(Args),
	    {200, Pixel,
	     [{content_type, "text/plain"}]};
        _Tokens ->
            501
    end.

parse_json_body(Request, Body) ->
    case rest_util:parse_body(
           Request, Body,
           [{jsone_options, [{object_format, proplist}]}]) of
        {error, _Reason} ->
            throw({error, <<"Invalid JSON format">>});
        JsonValue ->
            JsonValue
    end.




handle_http_response(Socket, Request, {json, Status, JsonValue})
  when is_integer(Status) ->
    json_response(Socket, Request, Status, default_phrase(Status), JsonValue);
handle_http_response(Socket, Request, {json, {Status, Phrase}, JsonValue}) ->
    json_response(Socket, Request, Status, Phrase, JsonValue);
handle_http_response(Socket, Request, Status) when is_integer(Status) ->
    response(Socket, Request, Status, default_phrase(Status));
handle_http_response(Socket, Request, {{Status, Phrase}})
  when is_integer(Status) ->
    response(Socket, Request, Status, Phrase);
handle_http_response(Socket, Request, {Status, ResponseBody})
  when is_integer(Status) ->
    response(Socket, Request, Status, default_phrase(Status), ResponseBody, []);
handle_http_response(Socket, Request, {{Status, Phrase}, ResponseBody}) ->
    response(Socket, Request, Status, Phrase, ResponseBody, []);
handle_http_response(Socket, Request, {Status, ResponseBody, Opts})
  when is_integer(Status) ->
    response(Socket, Request, Status, default_phrase(Status), ResponseBody,
             Opts);
handle_http_response(Socket, Request, {{Status, Phrase}, ResponseBody, Opts}) ->
    response(Socket, Request, Status, Phrase, ResponseBody, Opts).

json_response(Socket, Request, Status, Phrase, JsonValue) ->
    ResponseBody =
        jsone:encode(JsonValue,
                     [{float_format, [{decimals, 4}, compact]},
                      {indent, 2}, {object_key_type, value},
                      {space, 1}, native_forward_slash]),
    rester_http_server:response_r(
      Socket, Request, Status, Phrase, ResponseBody,
      [{content_type, "application/json"}]).

response(Socket, Request, Status, Phrase) ->
    rester_http_server:response_r(
      Socket, Request, Status, Phrase, {skip_body, 0}, []).

response(Socket, Request, Status, Phrase, Body, Opts) ->
    rester_http_server:response_r(Socket, Request, Status, Phrase, Body, Opts).



default_phrase(100) -> "Continue";
default_phrase(101) -> "Switching Protocols";
default_phrase(200) -> "OK";
default_phrase(201) -> "Created";
default_phrase(202) -> "Accepted";
default_phrase(203) -> "Non-Authoritative Information";
default_phrase(204) -> "No Content";
default_phrase(205) -> "Reset Content";
default_phrase(206) -> "Partial Content";
default_phrase(300) -> "Multiple Choices";
default_phrase(301) -> "Moved Permanently";
default_phrase(302) -> "Found";
default_phrase(303) -> "See Other";
default_phrase(304) -> "Not Modified";
default_phrase(305) -> "Use Proxy";
default_phrase(307) -> "Temporary Redirect";
default_phrase(400) -> "Bad Request";
default_phrase(401) -> "Unauthorized";
default_phrase(402) -> "Payment Required";
default_phrase(403) -> "Forbidden";
default_phrase(404) -> "Not Found";
default_phrase(405) -> "Method Not Allowed";
default_phrase(406) -> "Not Acceptable";
default_phrase(407) -> "Proxy Authentication Required";
default_phrase(408) -> "Request Time-out";
default_phrase(409) -> "Conflict";
default_phrase(410) -> "Gone";
default_phrase(411) -> "Length Required";
default_phrase(412) -> "Precondition Failed";
default_phrase(413) -> "Request Entity Too Large";
default_phrase(414) -> "Request-URI Too Large";
default_phrase(415) -> "Unsupported Media Type";
default_phrase(416) -> "Requested range not satisfiable";
default_phrase(417) -> "Expectation Failed";
default_phrase(500) -> "Internal Server Error";
default_phrase(501) -> "Not Implemented";
default_phrase(502) -> "Bad Gateway";
default_phrase(503) -> "Service Unavailable";
default_phrase(504) -> "Gateway Time-out";
default_phrase(505) -> "HTTP Version not supported".
