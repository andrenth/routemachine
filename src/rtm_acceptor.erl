-module(rtm_acceptor).
-behavior(gen_server).

-export([start_link/2]).

% Exports for gen_server.
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%         terminate/2, code_change/3]).
-export([init/1, handle_info/2, terminate/2]).

-include_lib("bgp.hrl").

-record(state, {
  listen_socket,
  fsm
}).

start_link(Port, FSM) ->
  SockOpts = [binary, {reuseaddr, true}, {packet, raw}, {active, false}],
  {ok, ListenSocket} = gen_tcp:listen(Port, SockOpts),
  State = #state{listen_socket = ListenSocket, fsm = FSM},
  gen_server:start_link(?MODULE, State, []).


%
% Callbacks for gen_server.
%

init(State) ->
  io:format("Starting acceptor ~w~n", [self()]),
  process_flag(trap_exit, true),
  {ok, State, 0}.

handle_info(timeout, #state{listen_socket = ListenSocket, fsm = FSM} = State) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  io:format("Returned from accept~n", []),
  {ok, Pid} = rtm_server_sup:start_child(FSM),
  gen_tcp:controlling_process(Socket, Pid),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State, 0}.

terminate(_Reason, #state{listen_socket = ListenSocket}) ->
  gen_tcp:close(ListenSocket),
  ok.
