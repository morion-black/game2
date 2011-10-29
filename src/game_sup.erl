
-module(game_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_game/2, start_user/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_game(Name, Options) ->
  supervisor:start_child(one_game_sup, [Name, Options]).

start_user(UserId, Options) ->
  supervisor:start_child(user_sup, [UserId, Options]).
  

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([one_game]) ->
  {ok, {{simple_one_for_one, 1000, 1000}, [
    {   undefined,                               % Id       = internal id
    {one_game,start_link,[]},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
    }
  ]}};


init([user]) ->
  {ok, {{simple_one_for_one, 1000, 1000}, [
    {   undefined,                               % Id       = internal id
    {user,start_link,[]},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
    }
  ]}};



init([]) ->
  Supervisors = [
  {   game_tracker_sup,
      {game_tracker,start_link,[]},
      permanent,                               % Restart  = permanent | transient | temporary
      1000,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                              % Type     = worker | supervisor
      [game_tracker]                                       % Modules  = [Module] | dynamic
  },
  {   one_game_sup,
      {supervisor,start_link,[{local, one_game_sup}, ?MODULE, [one_game]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
  },
  {   user_sup,
      {supervisor,start_link,[{local, user_sup}, ?MODULE, [user]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
  }
  
  ],
    {ok, { {one_for_one, 5, 10}, Supervisors} }.

















