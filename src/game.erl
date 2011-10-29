-module(game).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(cowboy),
	application:start(gproc),
  sync:go(),
	application:start(game).

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
			{[<<"comet">>], comet_handler, []},
			{[<<"game_info">>], game_info_handler, []},
			{'_', http_file_handler, [{root, "priv/wwwroot"}]}
		]}
	],
	cowboy:start_listener(http, 100,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	game_sup:start_link().

stop(_State) ->
    ok.
