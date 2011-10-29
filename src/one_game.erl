-module(one_game).

-behaviour(gen_server).

-export([start_link/2]).
-export([subscribe/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(Name, Options) ->
  gen_server:start_link(?MODULE, [Name, Options], []).


-record(game, {
  name,
  clients = [],
  options
}).

-record(client, {
  user_id,
  ref,
  pid
}).
  

subscribe(Game, UserId) ->
  gen_server:call(Game, {subscribe, UserId, self()}).
  
init([Name, Options]) ->
  put(our_name, Name),
  timer:send_interval(2000, check),
  {ok, #game{
    name = Name,
    options = Options
  }}.

handle_call({subscribe, UserId, Pid}, _From, #game{clients = Clients} = State) ->
  Ref = erlang:monitor(process, Pid),
  {reply, ok, State#game{clients = [#client{user_id = UserId, pid = Pid, ref = Ref}|Clients]}};

handle_call(_Call, _From, State) ->
  {stop, {unknown_call, _Call}, State}.


handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

handle_info(check, #game{clients = []} = Game) ->
  io:format("Exit game ~p~n", [Game#game.name]),
  {stop, normal, Game};

handle_info({'DOWN', _, process, User, _Reason}, #game{clients = Clients} = Game) ->
  Clients1 = lists:keydelete(User, #client.pid, Clients),
  {noreply, Game#game{clients = Clients1}};

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.
  
  
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
  
  
  
  
  
  
  
  
  
  
  