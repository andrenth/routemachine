-module(rtm_main).
-behavior(gen_server).

-include_lib("bgp.hrl").
-include_lib("session.hrl").

-export([start_link/2]).

% Exports for gen_server.
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%         terminate/2, code_change/3]).
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
         handle_cast/2]).

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

init(#state{peers = Peers} = State) ->
  process_flag(trap_exit, true),
  register(rtm_main, self()),
  start_active_sessions(Peers),
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
      rtm_fsm:trigger(Pid, start),
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

%
% Internal functions.
%

start_active_sessions(Peers) ->
  dict:fold(fun(_Ip, #session{establishment = Estab} = Session, ok) ->
    case Estab of
      active ->
        {ok, Pid} = rtm_fsm_sup:start_child(Session),
        rtm_fsm:trigger(Pid, start);
      passive ->
        ok
    end
  end, ok, Peers).