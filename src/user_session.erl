-module(user_session).

-behaviour(gen_server).

-export([start_link/2]).
-export([subscribe/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(UserId, Options) ->
  gen_server:start_link(?MODULE, [UserId, Options], []).

subscribe(Session, GameName) ->
  gen_server:call(Session, {subscribe, GameName}).

-record(session, {
  user_id,
  game
}).


init([UserId, _Options]) ->
  gproc:add_local_name({user_id,UserId}),
  put(user_id, UserId),
  {ok, #session{
    user_id = UserId
  }}.

handle_call({subscribe, GameName}, _From, #session{user_id = UserId} = Session) ->
  {ok, Pid} = game_tracker:open(GameName, UserId, []),
  {reply, {ok, Pid}, Session#session{game = Pid}};

handle_call(_Call, _From, State) ->
  {stop, {unknown_call, _Call}, State}.


handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

handle_info({'DOWN', _, process, User, _Reason}, #session{} = Session) ->
  {noreply, Session};

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
  
  
  
  
  
  
  
  
  
  
