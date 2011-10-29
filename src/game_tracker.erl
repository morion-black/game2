%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Example of gen_server
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
-module(game_tracker).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").


%% External API
-export([start_link/0]).
-export([find_or_create/2, open/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(state, {
  clients
}).

-record(entry, {
  name,
  ref,
  pid
}).


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

find_or_create(Name, Options) ->
  {ok,Pid_} = case ets:lookup(?MODULE, Name) of
    [] -> gen_server:call(?MODULE, {find_or_create,Name,Options});
    [#entry{pid = Pid}] -> {ok, Pid}
  end,
  {ok, Pid_}.


open(Name, UserId, Options) ->
  {ok, Pid} = find_or_create(Name, Options),
  one_game:subscribe(Pid, UserId),
  erlang:monitor(process, Pid),
  {ok, Pid}.

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([]) ->
  ets:new(?MODULE, [public,named_table,{keypos,#entry.name}]),

  {ok, #state{
  
  }}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call({find_or_create, Name, Options}, _From, #state{} = Tracker) ->
  Pid = case ets:lookup(?MODULE, Name) of
    [] ->
      {ok, Pid_} = game_sup:start_game(Name, Options),
      Ref = erlang:monitor(process, Pid_),
      ets:insert(?MODULE, #entry{name = Name, ref = Ref, pid = Pid_}),
      Pid_;
    [#entry{pid = Pid_}] ->
      Pid_
  end,
  {reply, {ok,Pid}, Tracker};

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({'DOWN', _, process, Game, _Reason}, #state{} = Tracker) ->
  ets:select_delete(?MODULE, ets:fun2ms(fun(#entry{pid = Pid}) when Game == Pid -> true end)),
  {noreply, Tracker};

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
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
