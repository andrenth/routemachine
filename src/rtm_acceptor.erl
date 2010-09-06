-module(rtm_acceptor).
-behavior(gen_server).

-export([start_link/2]).

% Exports for gen_server.
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%         terminate/2, code_change/3]).
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
         handle_cast/2]).

-include_lib("bgp.hrl").

-record(state, {
  listen_socket,
  peers
}).


start_link(ListenSocket, Peers) ->
  State = #state{listen_socket = ListenSocket, peers = Peers},
  gen_server:start_link(?MODULE, State, []).

%
% Callbacks for gen_server.
%

init(State) ->
  process_flag(trap_exit, true),
  {ok, State, 0}.

handle_info(timeout, #state{listen_socket = ListenSocket,
                            peers         = Peers} = State) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  {ok, {PeerAddr, _Port}} = inet:peername(Socket),
  case dict:find(PeerAddr, Peers) of
    {ok, S} ->
      Session = S#session{establishment = {passive, Socket}},
      {ok, Pid} = rtm_fsm_sup:start_child(Session),
      gen_tcp:controlling_process(Socket, Pid),
      gen_fsm:send_event(Pid, start),
      inet:setopts(Socket, [{active, once}]);
    error ->
      gen_tcp:close(Socket)
  end,
  {noreply, State, 0}.

terminate(_Reason, #state{listen_socket = ListenSocket}) ->
  gen_tcp:close(ListenSocket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {stop, unexpected_call, State}.

handle_cast(_Request, State) ->
  {stop, unexpected_cast, State}.
