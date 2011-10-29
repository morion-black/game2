-module(game_info_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.
  

handle(Req, State) ->
  {Name, Req2} = cowboy_http_req:qs_val(<<"game">>, Req),
  {ok, Game} = game_tracker:find_or_create(Name, []),
  {ok, Info} = one_game:info(Game),
  {ok, Req3} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], iolist_to_binary(mochijson2:encode(Info)), Req2),
  
  {ok, Req3, State}.

terminate(_Req, _State) ->
	ok.
