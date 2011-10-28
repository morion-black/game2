-module(comet_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.


handle(Req, State) ->
  timer:sleep(5000),
  {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"applicatio/javascript">>}], <<"true">>, Req),
  {ok, Req2, State}.

terminate(_Req, _State) ->
	ok.
