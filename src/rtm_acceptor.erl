-module(rtm_acceptor).
-behavior(gen_server).

-export([start_link/1]).

% Exports for gen_server.
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%         terminate/2, code_change/3]).
-export([init/1, handle_info/2, terminate/2]).

-include_lib("bgp.hrl").

start_link(ListenSocket) ->
  gen_server:start_link(?MODULE, ListenSocket, []).


%
% Callbacks for gen_server.
%

init(ListenSocket) ->
  io:format("Starting acceptor ~w~n", [self()]),
  process_flag(trap_exit, true),
  {ok, ListenSocket, 0}.

handle_info(timeout, ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  io:format("Returned from accept~n", []),
  {ok, Pid} = rtm_fsm_sup:start_child({passive, Socket}),
  gen_tcp:controlling_process(Socket, Pid),
  gen_fsm:send_event(Pid, start),
  inet:setopts(Socket, [{active, once}]),
  {noreply, ListenSocket, 0}.

terminate(_Reason, ListenSocket) ->
  gen_tcp:close(ListenSocket),
  ok.
