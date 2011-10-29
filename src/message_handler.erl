-module(message_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.
  

handle(Req, State) ->
  {UserIdBin, Req2} = cowboy_http_req:qs_val(<<"user_id">>, Req),
  {GameName, _} = cowboy_http_req:qs_val(<<"game">>, Req2),
  {Message, _} = cowboy_http_req:qs_val(<<"message">>, Req2),
  UserId = list_to_integer(binary_to_list(UserIdBin)),
  game_tracker:send_message(GameName, Message),
  {ok, Req3} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], <<"true">>, Req2),
  {ok, Req3, State}.

terminate(_Req, _State) ->
	ok.
