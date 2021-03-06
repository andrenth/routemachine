-module(rtm_acceptor).
-behavior(gen_server).

-include_lib("bgp.hrl").
-include_lib("session.hrl").

-export([start_link/2]).

% Exports for gen_server.
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

init(State) ->
  process_flag(trap_exit, true),
  register(?MODULE, self()),
  {ok, State, 0}.

handle_info(timeout, #state{listen_socket = ListenSocket,
                            peers         = Peers} = State) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  {ok, {PeerAddr, _Port}} = inet:peername(Socket),
  NewState =
    case dict:find(PeerAddr, Peers) of
      {ok, Session} ->
        NewSession = start_fsm(Socket, Session),
        State#state{peers = dict:store(PeerAddr, NewSession, Peers)};
      error ->
        error_logger:warning_msg("Connect attempt from bad peer ~p~n",
          [PeerAddr]),
        gen_tcp:close(Socket),
        State
    end,
  {noreply, NewState, 0}.

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

start_fsm(Socket, Session) ->
  start_fsm(Socket, Session, undefined).

start_fsm(Socket, #session{peer_addr = PeerAddr,
                           idle_time = IdleTime} = Session, Delay) ->
  PassiveSession = Session#session{establishment = {passive, Socket}},
  case rtm_fsm_sup:start_child(PassiveSession) of
    {ok, Pid} ->
      gen_tcp:controlling_process(Socket, Pid),
      trigger_start_event(Pid, Delay),
      % Don't return the passive session. If this session is closed,
      % we'll actively try to start a new one.
      Session;
    {error, already_present} ->
      ok = rtm_fsm_sup:delete_child({rtm_fsm, PeerAddr}),
      start_fsm(Socket, Session#session{idle_time = IdleTime * 2}, IdleTime);
    {error, {already_started, Pid}} ->
      error_logger:info_msg("Connection collision detected~n"),
      handle_collision(Socket, Session, Pid)
  end.

trigger_start_event(Pid, undefined) ->
  rtm_fsm:trigger(Pid, start);
trigger_start_event(Pid, Delay) ->
  error_logger:info_msg("Will trigger a start event in ~p seconds~n", [Delay]),
  timer:apply_after(timer:seconds(Delay), rtm_fsm, trigger, [Pid, start]).

handle_collision(Socket, #session{peer_addr   = PeerAddr,
                                  peer_bgp_id = PeerBgpId} = Session, Pid) ->
  case rtm_fsm:state(Pid) of
    established ->
      error_logger:info_msg("Rejecting active connection from peer ~p~n",
        [PeerAddr]),
      send_notification(Socket, ?BGP_ERR_CEASE),
      gen_tcp:close(Socket),
      Session;
    _Other ->
      % When the existing FSM is not in the established state,
      % prefer to use the incoming connection.
      error_logger:info_msg("Accepting active connection from peer ~p~n",
        [PeerAddr]),
      ok = rtm_fsm_sup:terminate_child(PeerBgpId),
      ok = rtm_fsm_sup:delete_child(PeerBgpId),
      start_fsm(Socket, Session)
  end.

send_notification(Socket, Error) ->
  gen_tcp:send(Socket, rtm_msg:build_notification(Error)).
