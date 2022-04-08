%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    REST client functions
%%% @end
%%% Created :  8 Apr 2022 by Tony Rogvall <tony@rogvall.se>

-module(buller_client).

-export([write_image/2, write_image/4]).

write_image(Url, Filename) ->
    write_image(Url, 0, 0, Filename).

write_image(Url, Xd, Yd, Filename) ->
    {ok,Image} = epx_image:load(Filename),
    [Pixmap|_] = epx_image:pixmaps(Image),
    lists:foreach(
      fun(Y) ->
	      lists:foreach(
		fun(X) ->
			Value = epx:pixmap_get_pixel(Pixmap, X, Y),
			upload_pixel(Url, Xd+X, Yd+Y, Value)
		end, lists:seq(0, epx:pixmap_info(Pixmap, width)-1))
      end, lists:seq(0, epx:pixmap_info(Pixmap, height)-1)).
		
upload_pixel(Url, X, Y, ARGB) ->
    Data = [{x, X}, {y, Y}, {color, list_to_binary(argb_to_hex(ARGB))}],
    rester_http:wpost(Url++"/pixel",
		      [{'Content-Type', "application/json"}],
		      Data).

argb_to_hex({A,R,G,B}) ->
    "#" ++ 
	tl(integer_to_list(16#100+R,16)) ++
	tl(integer_to_list(16#100+G,16)) ++
	tl(integer_to_list(16#100+B,16)) ++
	tl(integer_to_list(16#100+A,16)).
