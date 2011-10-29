-module(one_game).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(Name, Options) ->
  gen_server:start_link(?MODULE, [Name, Options], []).


-record(game, {
  name,
  options
}).
  
init([Name, Options]) ->
  put(our_name, Name),
  {ok, #game{
    name = Name,
    options = Options
  }}.


handle_call(_Call, _From, State) ->
  {stop, {unknown_call, _Call}, State}.


handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.
  

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
  
  
  
  
  
  
  
  
  
  
  