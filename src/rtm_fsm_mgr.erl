-module(rtm_fsm_mgr).
-behavior(gen_server).

-export([start_link/0]).

% API
-export([register/1, unregister/1]).

% Exports for gen_server.
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2,
         code_change/3]).

-include_lib("session.hrl").

-record(state, {
  fsms
}). 

start_link() ->
  gen_server:start_link(?MODULE, ok, []).

%
% API.
%

register(Pid) ->
  gen_server:call(rtm_fsm_mgr, {register, Pid}).

unregister(Pid) ->
  gen_server:call(rtm_fsm_mgr, {unregister, Pid}).

%
% Callbacks for gen_server.
%

init(ok) ->
  process_flag(trap_exit, true),
  register(rtm_fsm_mgr, self()),
  {ok, #state{fsms = sets:new()}}.

handle_call({register, Pid}, _From, #state{fsms = Fsms} = State) ->
  case sets:is_element(Pid, Fsms) of
    true  -> {reply, error, State};
    false -> {reply, ok, State#state{fsms = sets:add_element(Pid, Fsms)}}
  end;

handle_call({unregister, Pid}, _From, #state{fsms = Fsms} = State) ->
  {reply, ok, State#state{fsms = sets:del_element(Pid, Fsms)}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_cast(_Request, State) ->
  {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
  {stop, unexpected_info, State}.
