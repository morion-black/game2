-module(comet_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.
  

handle(Req, State) ->
  {UserIdBin, Req2} = cowboy_http_req:qs_val(<<"user_id">>, Req),
  {GameName, _} = cowboy_http_req:qs_val(<<"game">>, Req2),
  UserId = list_to_integer(binary_to_list(UserIdBin)),
  {ok, Session} = case gproc:lookup_local_name({user_id, UserId}) of
    undefined ->
      game_sup:start_user(UserId, []);
    Session_ ->
      {ok, Session_}
  end,
  io:format("Comet process: ~p~n", [self()]),
  user_session:subscribe(Session, GameName),
  {ok, Messages} = user_session:wait_message(Session),
  % io:format("Hi: ~p ~s ~p~n", [UserId, GameName, Messages]),
  {ok, Req3} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/javascript">>}], 
    iolist_to_binary(mochijson2:encode(Messages)), 
  Req2),
  {ok, Req3, State}.

terminate(_Req, _State) ->
	ok.
