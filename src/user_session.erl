-module(user_session).

-behaviour(gen_server).

-export([start_link/2]).
-export([subscribe/2, wait_message/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(UserId, Options) ->
  gen_server:start_link(?MODULE, [UserId, Options], []).

subscribe(Session, GameName) ->
  gen_server:call(Session, {subscribe, GameName}).

wait_message(Session) ->
  gen_server:call(Session, wait_message, 20000).

-record(session, {
  user_id,
  messages = [],
  wait_queue = [],
  last_seen_at,
  game
}).

-define(TIMEOUT, 12000).

init([UserId, _Options]) ->
  gproc:add_local_name({user_id,UserId}),
  put(user_id, UserId),
  timer:send_interval(?TIMEOUT, check),
  {ok, #session{
    last_seen_at = erlang:now(),
    user_id = UserId
  }}.

handle_call({subscribe, GameName}, _From, #session{user_id = UserId} = Session) ->
  {ok, Pid} = game_tracker:open(GameName, UserId, []),
  {reply, {ok, Pid}, Session#session{last_seen_at = erlang:now(), game = Pid}};


handle_call(wait_message, From, #session{messages = [], wait_queue = Queue} = Session) ->
  {noreply, Session#session{wait_queue = [From|Queue]}};

handle_call(wait_message, _From, #session{messages = Messages} = Session) ->
  {reply, {ok, Messages}, Session};

handle_call(_Call, _From, State) ->
  {stop, {unknown_call, _Call}, State}.


handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

handle_info(check, #session{last_seen_at = LastSeenAt} = Session) ->
  Delta = timer:now_diff(erlang:now(), LastSeenAt) div 1000,
  if
    Delta > 2*?TIMEOUT ->
      io:format("User session dies: ~p~n", [Session]),
      {stop, normal, Session};
    true ->
      {noreply, Session}
  end;

handle_info({message, Message}, #session{wait_queue = Queue, messages = Messages} = Session) ->
  Msg1 = [Message|Messages],
  if
    Queue == [] ->
      {noreply, Session#session{messages = Msg1}};
    true ->  
      [gen_server:reply(From, {ok, Msg1}) || From <- Queue],
      {noreply, Session#session{wait_queue = [], messages = []}}
  end;

handle_info({'DOWN', _, process, _User, _Reason}, #session{} = Session) ->
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
  
  
  
  
  
  
  
  
  
  
