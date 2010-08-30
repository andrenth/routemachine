-module(rtm_fsm).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1]).

% BGP FSM states.
-export([idle/2, connect/2, active/2, open_sent/2, open_confirm/2,
         established/2]).

% Other gen_fsm exports.
-export([handle_info/3]).

-include_lib("bgp.hrl").

start_link(Port) ->
  gen_fsm:start_link(?MODULE, Port, []).

init(Port) ->
  Session = #session{listen_port     = Port,
                     hold_time       = ?BGP_TIMER_HOLD,
                     keepalive_time  = ?BGP_TIMER_KEEPALIVE,
                     conn_retry_time = ?BGP_TIMER_CONN_RETRY},
  {ok, idle, Session}.

%
% BGP FSM.
%

% Idle state.

idle(start, Session) ->
  ConnRetry = start_timer(conn_retry, Session#session.conn_retry_time),
  {Event, NewSession} =
    connect_to_peer(Session#session{conn_retry_timer = ConnRetry}),
  gen_fsm:send_event(self(), Event),
  {next_state, connect, NewSession};

idle(_Error, Session) ->
  % TODO exponential backoff for reconnection attempt.
  NewSession = close_connection(Session),
  {next_state, idle, NewSession}.


% Connect state.

connect(start, Session) ->
  {next_state, connect, Session};

connect(tcp_open, Session) ->
  clear_timer(Session#session.conn_retry_timer),
  rtm_msg:send_open(Session),
  {next_state, active, Session};

connect(tcp_open_failed, Session) ->
  ConnRetry = restart_timer(conn_retry, Session),
  NewSession = close_connection(Session),
  {next_state, active, NewSession#session{conn_retry_timer = ConnRetry}};

connect({timeout, _Ref, conn_retry}, Session) ->
  ConnRetry = restart_timer(conn_retry, Session),
  connect_to_peer(Session#session{conn_retry_timer = ConnRetry}),
  {next_state, connect, Session};

connect(_Event, Session) ->
  NewSession = release_resources(Session),
  {next_state, idle, NewSession}.


% Active state.

active(start, Session) ->
  {next_state, active, Session};

active(tcp_open, Session) ->
  case check_peer(Session) of
    ok ->
      clear_timer(Session#session.conn_retry_timer),
      rtm_msg:send_open(Session),
      HoldTimer = start_timer(hold, Session#session.hold_time),
      {next_state, open_sent, Session#session{hold_timer = HoldTimer}};
    bad_peer ->
      ConnRetry = restart_timer(conn_retry, Session),
      NewSession =
        close_connection(Session#session{conn_retry_timer = ConnRetry}),
      {next_state, active, NewSession}
  end;

active({timeout, _Ref, conn_retry}, Session) ->
  ConnRetry = restart_timer(conn_retry, Session),
  NewSession = connect_to_peer(Session#session{conn_retry_timer = ConnRetry}),
  {next_state, connect, NewSession};

active(_Event, Session) ->
  NewSession = release_resources(Session),
  {next_state, idle, NewSession}.


% OpenSent state

open_sent(start, Session) ->
  {next_state, open_sent, Session};

open_sent(stop, Session) ->
  rtm_msg:send_notification(Session, ?BGP_ERR_CEASE),
  release_resources(Session),
  {next_state, idle, Session};

open_sent(open_received, Session) ->
  case rtm_msg:parse_open(Session) of
    {ok, ASN, HoldTime, _BGPId} ->
      rtm_msg:send_keepalive(Session),
      NewHoldTime = negotiate_hold_time(Session#session.hold_time, HoldTime),
      NewSession = start_timers(Session, NewHoldTime),
      {next_state, open_confirm, NewSession#session{remote_asn = ASN}};
    {error, Error} ->
      rtm_msg:send_notification(Session, Error),
      release_resources(Session),
      {next_state, idle, Session}
  end;

open_sent({timeout, _Ref, hold}, Session) ->
  rtm_msg:send_notification(Session, ?BGP_ERR_HOLD_TIME),
  release_resources(Session),
  {next_state, idle, Session};

open_sent(_Event, Session) ->
  rtm_msg:send_notification(Session, ?BGP_ERR_FSM),
  release_resources(Session),
  {next_state, idle, Session}.


% OpenConfirm state.

open_confirm(start, Session) ->
  {next_state, open_confirm, Session};

open_confirm(stop, Session) ->
  rtm_msg:send_notification(Session, ?BGP_ERR_CEASE),
  release_resources(Session),
  {next_state, idle, Session};

open_confirm(keepalive_received, Session) ->
  {next_state, established, Session};

open_confirm({timeout, hold}, Session) ->
  rtm_msg:send_notification(Session, ?BGP_ERR_HOLD_TIME),
  {next_state, idle, Session};

open_confirm(notification_received, Session) ->
  {next_state, idle, Session};

open_confirm({timeout, keepalive}, Session) ->
  KeepAlive = restart_timer(keepalive, Session),
  rtm_msg:send_keepalive(Session),
  {next_state, open_confirm, Session#session{keepalive_timer = KeepAlive}};

open_confirm(_Event, Session) ->
  rtm_msg:send_notification(Session, ?BGP_ERR_FSM),
  release_resources(Session),
  {next_state, idle, Session}.


% Established state.

established(start, Session) ->
  {next_state, established, Session};

established(stop, Session) ->
  rtm_msg:send_notification(Session, ?BGP_ERR_CEASE),
  release_resources(Session),
  {next_state, idle, Session};

established(update_received, Session) ->
  Hold = restart_timer(hold, Session),
  NewSession = Session#session{hold_timer = Hold},
  % TODO handle update - section 6.3.
  case rtm_msg:parse_update(NewSession) of
    ok ->
      {next_state, established, NewSession};
    {error, _Error} ->
      rtm_msg:send_notification(NewSession, ?BGP_ERR_UPDATE),
      release_resources(NewSession),
      {next_state, idle, NewSession}
  end;

established(keepalive_received, Session) ->
  Hold = restart_timer(hold, Session),
  {next_state, established, Session#session{hold_timer = Hold}};

established(notification_received, Session) ->
  release_resources(Session),
  {next_state, idle, Session};

established({timeout, hold}, Session) ->
  rtm_msg:send_notification(Session, ?BGP_ERR_HOLD_TIME),
  release_resources(Session),
  {next_state, idle, Session};

established({timeout, keepalive}, Session) ->
  KeepAlive = restart_timer(keepalive, Session),
  rtm_msg:send_keepalive(Session),
  {next_state, established, Session#session{keepalive_timer = KeepAlive}};

established(_Event, Session) ->
  rtm_msg:send_notification(Session, ?BGP_ERR_FSM),
  release_resources(Session),
  % TODO delete_routes(Session),
  {next_state, idle, Session}.


% Other callbacks.

% Connection closed by peer while in different states.

handle_info({tcp_closed, _Socket}, open_sent, Session) ->
  close_connection(Session),
  ConnRetry = restart_timer(conn_retry, Session),
  {next_state, active, Session#session{conn_retry_timer = ConnRetry}};

handle_info({tcp_error, _Socket}, open_sent, Session) ->
  NewSession = release_resources(Session),
  {next_state, idle, NewSession}.


handle_info({tcp_closed, _Socket}, open_confirm, Session) ->
  NewSession = release_resources(Session),
  {next_state, idle, NewSession};

handle_info({tcp_error, _Socket}, open_confirm, Session) ->
  NewSession = release_resources(Session),
  {next_state, idle, NewSession};


handle_info({tcp_closed, _Socket}, established, Session) ->
  NewSession = release_resources(Session),
  {next_state, idle, Session};

handle_info({tcp_error, _Socket}, established, Session) ->
  NewSession = release_resources(Session),
  {next_state, idle, Session}.


%
% Internal functions.
%

restart_timer(conn_retry, Session) ->
  clear_timer(Session#session.conn_retry_timer),
  start_timer(conn_retry, Session#session.conn_retry_time);

restart_timer(keepalive, Session) ->
  clear_timer(Session#session.keepalive_timer),
  start_timer(keepalive, Session#session.keepalive_time);

restart_timer(hold, #session{hold_time = HoldTime} = Session) ->
  clear_timer(Session#session.hold_timer),
  case HoldTime > 0 of
    true  -> start_timer(keepalive, Session#session.keepalive_time);
    false -> undefined
  end.

start_timer(Name, Time) ->
  gen_fsm:start_timer(Time, Name).

clear_timer(Timer) ->
  gen_fsm:cancel_timer(Timer).

% Instead of implementing collision detection, just make sure there's
% only one connection per peer. This is what OpenBGPd does.
connect_to_peer(#session{socket = undefined} = Session) ->
  ListenPort = Session#session.listen_port,
  LocalAddr  = Session#session.local_addr,
  RemoteAddr = Session#session.remote_addr,
  {ok, _Pid} = rtm_acceptor_sup:start_child(self(), ListenPort),
  SockOpts = [binary, {ip, LocalAddr}, {packet, raw}, {active, false}],
  case gen_tcp:connect(RemoteAddr, ListenPort, SockOpts) of
    {ok, Socket}     -> {tcp_open, Session#session{socket = Socket}};
    {error, _Reason} -> {tcp_open_failed, Session}
  end;

% Already has a socket; ignore.
connect_to_peer(Session) ->
  Session.

close_connection(#session{socket = Socket} = Session) ->
  gen_tcp:close(Socket),
  Session#session{socket = undefined}.

release_resources(Session) ->
  NewSession = close_connection(Session),
  clear_timer(Session#session.conn_retry_timer),
  clear_timer(Session#session.hold_timer),
  clear_timer(Session#session.keepalive_timer),
  NewSession.

check_peer(#session{socket = Socket, remote_addr = RemoteAddr}) ->
  {ok, {Addr, _Port}} = inet:peername(Socket),
  case Addr of
    RemoteAddr -> ok;
    _          -> bad_peer
  end.

negotiate_hold_time(LocalHoldTime, RemoteHoldTime) ->
  HoldTime = min(LocalHoldTime, RemoteHoldTime),
  case HoldTime < ?BGP_TIMER_HOLD_MIN of
    true  -> 0;
    false -> HoldTime
  end.

start_timers(Session, 0) ->
  Session;
start_timers(Session, HoldTime) ->
  KeepAlive = start_timer(keepalive, Session#session.keepalive_time),
  Hold = start_timer(hold, HoldTime),
  Session#session{hold_time       = HoldTime,
                  hold_timer      = Hold,
                  keepalive_timer = KeepAlive}.
