-module(one_game).

-behaviour(gen_server).

-export([start_link/2]).
-export([subscribe/2, info/1, send_message/2, move/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(Name, Options) ->
  gen_server:start_link(?MODULE, [Name, Options], []).


-record(game, {
  name,
  size = 3,
  slots = [],
  clients = [],
  options
}).

-record(client, {
  user_id,
  ref,
  pid
}).


send_message(Game, Message) ->
  gen_server:call(Game, {message, Message}).

subscribe(Game, UserId) ->
  (catch gen_server:call(Game, {subscribe, UserId, self()})).

move(Game, UserId, X, Y) ->
  gen_server:call(Game, {move, UserId, X, Y}).
  
info(Game) ->
  gen_server:call(Game, info).
  
init([Name, Options]) ->
  Size = proplists:get_value(size, Options, 3),
  Slots = array:new(Size*Size),
  put(our_name, Name),
  timer:send_interval(20000, check),
  {ok, #game{
    name = Name,
    size = Size,
    slots = Slots,
    options = Options
  }}.
  
handle_call({move, _UserId, X, Y}, _From, #game{size = Size} = State) when 
  not is_number(X) orelse X < 0 orelse X >= Size orelse
  not is_number(Y) orelse Y < 0 orelse Y >= Size ->
  {reply, {error, invalid_move}, State};  

handle_call({move, UserId, X, Y}, _From, #game{slots = Slots, size = Size, clients = Clients} = State) ->
  Offset = Y*Size + X,
  case array:get(Offset, Slots) of
    undefined ->
      Slots1 = array:set(Offset, UserId, Slots),
      Msg = [{move, [{user_id,UserId},{x,X},{y,Y}]}],
      [Pid ! {message, Msg} || #client{pid = Pid} <- Clients],
      case has_victory(UserId, Slots1, Size) of
        true ->
          [Pid ! {message, [{winner,UserId}]} || #client{pid = Pid} <- Clients],
          {reply, {ok, true}, State#game{slots = Slots}};
        false ->
          {reply, {ok, true}, State#game{slots = Slots1}}
      end;
    _Else ->
      {reply, {error, slot_locked}, State}
  end;

handle_call({subscribe, UserId, Pid}, _From, #game{clients = Clients} = State) ->
  case lists:keyfind(Pid, #client.pid, Clients) of
    #client{} ->
      {reply, ok, State};
    _ ->  
      Ref = erlang:monitor(process, Pid),
      {reply, ok, State#game{clients = [#client{user_id = UserId, pid = Pid, ref = Ref}|Clients]}}
  end;

handle_call({message, Message}, _From, #game{clients = Clients} = State) ->
  io:format("Clients of game: ~p~n", [Clients]),
  [Pid ! {message, Message} || #client{pid = Pid} <- Clients],
  {reply, ok, State};

handle_call(info, _From, #game{slots = Slots, size = Size} = State) ->
  Info = [{size, Size}, {slots, array:to_list(Slots)}],
  {reply, {ok, Info}, State};

handle_call(_Call, _From, State) ->
  {stop, {unknown_call, _Call}, State}.


handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

handle_info(check, #game{clients = []} = Game) ->
  io:format("Exit game ~p~n", [Game#game.name]),
  {stop, normal, Game};

handle_info(check, #game{} = Game) ->
  {noreply, Game};

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
  
  
has_victory(UserId, Slots, _Size) ->
  victory_horiz(UserId, Slots, 0) orelse
  victory_horiz(UserId, Slots, 3) orelse
  victory_horiz(UserId, Slots, 6) orelse
  victory_vert(UserId, Slots, 0) orelse
  victory_vert(UserId, Slots, 1) orelse
  victory_vert(UserId, Slots, 2) orelse
  victory_diag1(UserId, Slots) orelse
  victory_diag2(UserId, Slots).


victory_horiz(UserId, Slots, Start) ->
  array:get(Start, Slots) == UserId andalso
  array:get(Start+1, Slots) == UserId andalso
  array:get(Start+2, Slots) == UserId.

victory_vert(UserId, Slots, Start) ->
  array:get(Start, Slots) == UserId andalso
  array:get(Start+3, Slots) == UserId andalso
  array:get(Start+6, Slots) == UserId.
  
victory_diag1(UserId, Slots) ->
  array:get(0, Slots) == UserId andalso
  array:get(4, Slots) == UserId andalso
  array:get(8, Slots) == UserId.
  

victory_diag2(UserId, Slots) ->
  array:get(2, Slots) == UserId andalso
  array:get(4, Slots) == UserId andalso
  array:get(6, Slots) == UserId.
  
  
  
  
  