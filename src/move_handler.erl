-module(move_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.
  

handle(Req, State) ->
  {UserIdBin, Req2} = cowboy_http_req:qs_val(<<"user_id">>, Req),
  {GameName, _} = cowboy_http_req:qs_val(<<"game">>, Req2),
  {XBin, _} = cowboy_http_req:qs_val(<<"x">>, Req2),
  {YBin, _} = cowboy_http_req:qs_val(<<"y">>, Req2),
  UserId = list_to_integer(binary_to_list(UserIdBin)),
  X = list_to_integer(binary_to_list(XBin)),
  Y = list_to_integer(binary_to_list(YBin)),
  {ok, Game} = game_tracker:find_or_create(GameName, []),
  {ok, Reply} = one_game:move(Game, UserId, X, Y),
  % io:format("Hi: ~p ~s ~p~n", [UserId, GameName, Messages]),
  {ok, Req3} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], 
    iolist_to_binary(mochijson2:encode(Reply)), 
  Req2),
  {ok, Req3, State}.

terminate(_Req, _State) ->
	ok.
