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

-record(peer, {
  socket,
  listen_port,
  message,
  type,
  local_asn,
  remote_asn,
  local_addr,
  remote_addr,
  hold_time,
  hold_timer,
  keepalive_time,
  keepalive_timer,
  conn_retry_time,
  conn_retry_timer
}).

start_link(Port) ->
  gen_fsm:start_link(?MODULE, Port, []).

init(Port) ->
  Peer = #peer{listen_port     = Port,
               hold_time       = ?BGP_TIMER_HOLD,
               keepalive_time  = ?BGP_TIMER_KEEPALIVE,
               conn_retry_time = ?BGP_TIMER_CONN_RETRY},
  {ok, idle, Peer}.

%
% BGP FSM.
%

% Idle state.

idle(start, Peer) ->
  ConnRetry = start_timer(conn_retry, Peer#peer.conn_retry_time),
  {Event, NewPeer} = connect_to_peer(Peer#peer{conn_retry_timer = ConnRetry}),
  gen_fsm:send_event(self(), Event),
  {next_state, connect, NewPeer};

idle(_Error, Peer) ->
  % TODO exponential backoff for reconnection attempt.
  NewPeer = close_connection(Peer),
  {next_state, idle, NewPeer}.


% Connect state.

connect(start, Peer) ->
  {next_state, connect, Peer};

connect(tcp_open, Peer) ->
  clear_timer(Peer#peer.conn_retry_timer),
  rtm_msg:send_open(Peer),
  {next_state, active, Peer};

connect(tcp_open_failed, Peer) ->
  ConnRetry = restart_timer(conn_retry, Peer),
  NewPeer = close_connection(Peer),
  {next_state, active, NewPeer#peer{conn_retry_timer = ConnRetry}};

connect({timeout, _Ref, conn_retry}, Peer) ->
  ConnRetry = restart_timer(conn_retry, Peer),
  connect_to_peer(Peer#peer{conn_retry_timer = ConnRetry}),
  {next_state, connect, Peer};

connect(_Event, Peer) ->
  NewPeer = release_resources(Peer),
  {next_state, idle, NewPeer}.


% Active state.

active(start, Peer) ->
  {next_state, active, Peer};

active(tcp_open, Peer) ->
  case check_peer(Peer) of
    ok ->
      clear_timer(Peer#peer.conn_retry_timer),
      rtm_msg:send_open(Peer),
      HoldTimer = start_timer(hold, Peer#peer.hold_time),
      {next_state, open_sent, Peer#peer{hold_timer = HoldTimer}};
    bad_peer ->
      ConnRetry = restart_timer(conn_retry, Peer),
      NewPeer = close_connection(Peer#peer{conn_retry_timer = ConnRetry}),
      {next_state, active, NewPeer}
  end;

active({timeout, _Ref, conn_retry}, Peer) ->
  ConnRetry = restart_timer(conn_retry, Peer),
  NewPeer = connect_to_peer(Peer#peer{conn_retry_timer = ConnRetry}),
  {next_state, connect, NewPeer};

active(_Event, Peer) ->
  NewPeer = release_resources(Peer),
  {next_state, idle, NewPeer}.


% OpenSent state

open_sent(start, Peer) ->
  {next_state, open_sent, Peer};

open_sent(stop, Peer) ->
  rtm_msg:send_notification(Peer, ?BGP_ERR_CEASE),
  release_resources(Peer),
  {next_state, idle, Peer};

open_sent(open_received, Peer) ->
  case rtm_msg:parse_open(Peer) of
    {ok, ASN, HoldTime, _BGPId} ->
      rtm_msg:send_keepalive(Peer),
      NewHoldTime = negotiate_hold_time(Peer#peer.hold_time, HoldTime),
      NewPeer = start_timers(Peer, NewHoldTime),
      {next_state, open_confirm, NewPeer#peer{remote_asn = ASN}};
    {error, Error} ->
      rtm_msg:send_notification(Peer, Error),
      release_resources(Peer),
      {next_state, idle, Peer}
  end;

open_sent({timeout, _Ref, hold}, Peer) ->
  rtm_msg:send_notification(Peer, ?BGP_ERR_HOLD_TIME),
  release_resources(Peer),
  {next_state, idle, Peer};

open_sent(_Event, Peer) ->
  rtm_msg:send_notification(Peer, ?BGP_ERR_FSM),
  release_resources(Peer),
  {next_state, idle, Peer}.


% OpenConfirm state.

open_confirm(start, Peer) ->
  {next_state, open_confirm, Peer};

open_confirm(stop, Peer) ->
  rtm_msg:send_notification(Peer, ?BGP_ERR_CEASE),
  release_resources(Peer),
  {next_state, idle, Peer};

open_confirm(keepalive_received, Peer) ->
  {next_state, established, Peer};

open_confirm({timeout, hold}, Peer) ->
  rtm_msg:send_notification(Peer, ?BGP_ERR_HOLD_TIME),
  {next_state, idle, Peer};

open_confirm(notification_received, Peer) ->
  {next_state, idle, Peer};

open_confirm({timeout, keepalive}, Peer) ->
  KeepAlive = restart_timer(keepalive, Peer),
  rtm_msg:send_keepalive(Peer),
  {next_state, open_confirm, Peer#peer{keepalive_timer = KeepAlive}};

open_confirm(_Event, Peer) ->
  rtm_msg:send_notification(Peer, ?BGP_ERR_FSM),
  release_resources(Peer),
  {next_state, idle, Peer}.


% Established state.

established(start, Peer) ->
  {next_state, established, Peer};

established(stop, Peer) ->
  rtm_msg:send_notification(Peer, ?BGP_ERR_CEASE),
  release_resources(Peer),
  {next_state, idle, Peer};

established(update_received, Peer) ->
  Hold = restart_timer(hold, Peer),
  NewPeer = Peer#peer{hold_timer = Hold},
  % TODO handle update - section 6.3.
  case rtm_msg:parse_update(NewPeer) of
    ok ->
      {next_state, established, NewPeer};
    {error, _Error} ->
      rtm_msg:send_notification(NewPeer, ?BGP_ERR_UPDATE),
      release_resources(NewPeer),
      {next_state, idle, NewPeer}
  end;

established(keepalive_received, Peer) ->
  Hold = restart_timer(hold, Peer),
  {next_state, established, Peer#peer{hold_timer = Hold}};

established(notification_received, Peer) ->
  release_resources(Peer),
  {next_state, idle, Peer};

established({timeout, hold}, Peer) ->
  rtm_msg:send_notification(Peer, ?BGP_ERR_HOLD_TIME),
  release_resources(Peer),
  {next_state, idle, Peer};

established({timeout, keepalive}, Peer) ->
  KeepAlive = restart_timer(keepalive, Peer),
  rtm_msg:send_keepalive(Peer),
  {next_state, established, Peer#peer{keepalive_timer = KeepAlive}};

established(_Event, Peer) ->
  rtm_msg:send_notification(Peer, ?BGP_ERR_FSM),
  release_resources(Peer),
  % TODO delete_routes(Peer),
  {next_state, idle, Peer}.


% Other callbacks.

% Connection closed by peer while in different states.

handle_info({tcp_closed, _Socket}, open_sent, Peer) ->
  close_connection(Peer),
  ConnRetry = restart_timer(conn_retry, Peer),
  {next_state, active, Peer#peer{conn_retry_timer = ConnRetry}};

handle_info({tcp_closed, _Socket}, open_confirm, Peer) ->
  close_connection(Peer),
  {next_state, idle, Peer};

handle_info({tcp_closed, _Socket}, established, Peer) ->
  close_connection(Peer),
  {next_state, idle, Peer}.


%
% Internal functions.
%

restart_timer(conn_retry, Peer) ->
  clear_timer(Peer#peer.conn_retry_timer),
  start_timer(conn_retry, Peer#peer.conn_retry_time);

restart_timer(keepalive, Peer) ->
  clear_timer(Peer#peer.keepalive_timer),
  start_timer(keepalive, Peer#peer.keepalive_time);

restart_timer(hold, #peer{hold_time = HoldTime} = Peer) ->
  clear_timer(Peer#peer.hold_timer),
  case HoldTime > 0 of
    true  -> start_timer(keepalive, Peer#peer.keepalive_time);
    false -> undefined
  end.

start_timer(Name, Time) ->
  gen_fsm:start_timer(Time, Name).

clear_timer(Timer) ->
  gen_fsm:cancel_timer(Timer).

% Instead of implementing collision detection, just make sure there's
% only one connection per peer. This is what OpenBGPd does.
connect_to_peer(#peer{socket = undefined} = Peer) ->
  ListenPort = Peer#peer.listen_port,
  LocalAddr  = Peer#peer.local_addr,
  RemoteAddr = Peer#peer.remote_addr,
  {ok, _Pid} = rtm_acceptor_sup:start_child(self(), ListenPort),
  SockOpts = [binary, {ip, LocalAddr}, {packet, raw}, {active, false}],
  case gen_tcp:connect(RemoteAddr, ListenPort, SockOpts) of
    {ok, Socket}     -> {tcp_open, Peer#peer{socket = Socket}};
    {error, _Reason} -> {tcp_open_failed, Peer}
  end;

% Already has a socket; ignore.
connect_to_peer(Peer) ->
  Peer.

close_connection(#peer{socket = Socket} = Peer) ->
  gen_tcp:close(Socket),
  Peer#peer{socket = undefined}.

release_resources(Peer) ->
  NewPeer = close_connection(Peer),
  clear_timer(Peer#peer.conn_retry_timer),
  clear_timer(Peer#peer.hold_timer),
  clear_timer(Peer#peer.keepalive_timer),
  NewPeer.

check_peer(#peer{socket = Socket, remote_addr = RemoteAddr}) ->
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

start_timers(Peer, 0) ->
  Peer;
start_timers(Peer, HoldTime) ->
  KeepAlive = start_timer(keepalive, Peer#peer.keepalive_time),
  Hold = start_timer(hold, HoldTime),
  Peer#peer{hold_time       = HoldTime,
            hold_timer      = Hold,
            keepalive_timer = KeepAlive}.
