-module(rtm_fsm).
-behaviour(gen_fsm).

-include_lib("bgp.hrl").
-include_lib("route.hrl").
-include_lib("session.hrl").

-export([start_link/1]).
-export([init/1]).

% API
-export([trigger/2, state/1]).

% BGP FSM states.
-export([idle/2, connect/2, active/2, open_sent/2, open_confirm/2,
         established/2]).

% Exports for gen_fsm.
-export([terminate/3, code_change/4, handle_event/3, handle_sync_event/4,
         handle_info/3]).

start_link(Session) ->
  gen_fsm:start_link(?MODULE, Session, []).

init(#session{establishment = Estab} = Session) ->
  case Estab of
    active ->
      trigger(self(), start),
      {ok, idle, Session};
    {passive, _Port} ->
      {ok, idle, Session}
  end.

%
% API
%

trigger(FSM, Event) ->
  gen_fsm:send_event(FSM, Event).

state(FSM) ->
  gen_fsm:sync_send_all_state_event(FSM, state).

%
% BGP FSM.
%

% Idle state.

idle(start, #session{establishment = Estab} = Session) ->
  ConnRetry = start_timer(conn_retry, Session#session.conn_retry_time),
  NewSession = Session#session{conn_retry_timer = ConnRetry},
  case Estab of
    active ->
      error_logger:info_msg("FSM:idle/start(active)~n"),
      EstabSession = connect_to_peer(NewSession),
      {next_state, connect, EstabSession};
    {passive, Socket} ->
      error_logger:info_msg("FSM:idle/start(passive)~n"),
      {ok, Pid} = rtm_server_sup:start_child(self()),
      inet:setopts(Socket, [{active, once}]),
      gen_tcp:controlling_process(Socket, Pid),
      {next_state, active, NewSession#session{server = Pid}}
  end;

idle(_Error, Session) ->
  error_logger:info_msg("FSM:idle/error(~p)~n", [_Error]),
  NewSession = close_connection(Session),
  {next_state, idle, NewSession}.


% Connect state.

connect(start, Session) ->
  error_logger:info_msg("FSM:connect/start~n"),
  {next_state, connect, Session};

connect(tcp_open, Session) ->
  error_logger:info_msg("FSM:connect/tcp_open~n"),
  clear_timer(Session#session.conn_retry_timer),
  send_open(Session),
  {next_state, open_sent, Session};

connect(tcp_open_failed, Session) ->
  error_logger:info_msg("FSM:connect/tcp_open_failed~n"),
  ConnRetry = restart_timer(conn_retry, Session),
  NewSession = close_connection(Session),
  {next_state, active, NewSession#session{conn_retry_timer = ConnRetry}};

connect({timeout, _Ref, conn_retry}, Session) ->
  error_logger:info_msg("FSM:connect/conn_retry_timeout~n"),
  ConnRetry = restart_timer(conn_retry, Session),
  NewSession = connect_to_peer(Session#session{conn_retry_timer = ConnRetry}),
  {next_state, connect, NewSession};

connect(_Event, Session) ->
  error_logger:info_msg("FSM:connect/other(~p)~n", [_Event]),
  {stop, normal, Session}.


% Active state.

active(start, Session) ->
  error_logger:info_msg("FSM:active/start~n"),
  {next_state, active, Session};

active(tcp_open, Session) ->
  error_logger:info_msg("FSM:active/tcp_open~n"),
  case check_peer(Session) of
    ok ->
      clear_timer(Session#session.conn_retry_timer),
      send_open(Session),
      HoldTimer = start_timer(hold, Session#session.hold_time),
      {next_state, open_sent, Session#session{hold_timer = HoldTimer}};
    bad_peer ->
      ConnRetry = restart_timer(conn_retry, Session),
      NewSession =
        close_connection(Session#session{conn_retry_timer = ConnRetry}),
      {next_state, active, NewSession}
  end;

active({open_received, Bin, Marker},
       #session{peer_asn = PeerASN, peer_addr = PeerAddr} = Session) ->
  error_logger:info_msg("FSM:active/open_received~n"),
  clear_timer(Session#session.conn_retry_timer),
  case rtm_parser:parse_open(Bin, Marker, PeerASN, PeerAddr) of
    {ok, #bgp_open{asn = ASN, bgp_id = BGPId, hold_time = HoldTime}} ->
      send_open(Session),
      send_keepalive(Session),
      NewHoldTime = negotiate_hold_time(Session#session.hold_time, HoldTime),
      NewSession = start_timers(Session, NewHoldTime),
      {next_state, open_confirm, NewSession#session{peer_asn    = ASN,
                                                    peer_bgp_id = BGPId}};
    {error, Error} ->
      log_error(Error),
      send_notification(Session, Error),
      {stop, normal, Session}
  end;

active({timeout, _Ref, conn_retry}, Session) ->
  error_logger:info_msg("FSM:active/conn_retry_timeout~n"),
  ConnRetry = restart_timer(conn_retry, Session),
  NewSession = connect_to_peer(Session#session{conn_retry_timer = ConnRetry}),
  {next_state, connect, NewSession};

active(_Event, Session) ->
  error_logger:info_msg("FSM:active/other(~p)~n", [_Event]),
  {stop, normal, Session}.


% OpenSent state

open_sent(start, Session) ->
  error_logger:info_msg("FSM:open_sent/start~n"),
  {next_state, open_sent, Session};

open_sent(stop, Session) ->
  error_logger:info_msg("FSM:open_sent/stop~n"),
  send_notification(Session, ?BGP_ERR_CEASE),
  {stop, normal, Session};

open_sent({open_received, Bin, Marker},
          #session{peer_asn = PeerASN, peer_addr = PeerAddr} = Session) ->
  error_logger:info_msg("FSM:open_sent/open_received~n"),
  case rtm_parser:parse_open(Bin, Marker, PeerASN, PeerAddr) of
    {ok, #bgp_open{asn = ASN, bgp_id = BGPId, hold_time = HoldTime}} ->
      send_keepalive(Session),
      NewHoldTime = negotiate_hold_time(Session#session.hold_time, HoldTime),
      NewSession = start_timers(Session, NewHoldTime),
      {next_state, open_confirm, NewSession#session{peer_asn    = ASN,
                                                    peer_bgp_id = BGPId}};
    {error, Error} ->
      log_error(Error),
      send_notification(Session, Error),
      {stop, normal, Session}
  end;

open_sent({timeout, _Ref, hold}, Session) ->
  error_logger:info_msg("FSM:open_sent/hold_timeout~n"),
  send_notification(Session, ?BGP_ERR_HOLD_TIME),
  {stop, normal, Session};

open_sent(tcp_closed, Session) ->
  error_logger:info_msg("FSM:open_sent/tcp_closed~n"),
  close_connection(Session),
  ConnRetry = restart_timer(conn_retry, Session),
  {next_state, active, Session#session{conn_retry_timer = ConnRetry}};

open_sent(tcp_fatal, Session) ->
  {stop, normal, Session};

open_sent(_Event, Session) ->
  error_logger:info_msg("FSM:open_sent/other(~p)~n", [_Event]),
  send_notification(Session, ?BGP_ERR_FSM),
  {stop, normal, Session}.


% OpenConfirm state.

open_confirm(start, Session) ->
  error_logger:info_msg("FSM:open_confirm/start~n"),
  {next_state, open_confirm, Session};

open_confirm(stop, Session) ->
  error_logger:info_msg("FSM:open_confirm/stop~n"),
  send_notification(Session, ?BGP_ERR_CEASE),
  {stop, normal, Session};

open_confirm(keepalive_received, #session{} = Session) ->
  error_logger:info_msg("FSM:open_confirm/keepalive_received~n"),
  % Going into established; join the process group and send our
  % installed routes to the peer.
  join_established_group(Session),
  distribute_installed_routes(Session),
  {next_state, established, Session};

open_confirm({timeout, _Ref, hold}, Session) ->
  error_logger:info_msg("FSM:open_confirm/hold_timeout~n"),
  send_notification(Session, ?BGP_ERR_HOLD_TIME),
  {next_state, idle, Session};

open_confirm({notification_received, Bin}, Session) ->
  error_logger:info_msg("FSM:open_confirm/notification_received~n"),
  log_notification(Bin),
  {next_state, idle, Session};

open_confirm({timeout, _Ref, keepalive}, Session) ->
  error_logger:info_msg("FSM:open_confirm/keepalive_timeout~n"),
  KeepAlive = restart_timer(keepalive, Session),
  send_keepalive(Session),
  {next_state, open_confirm, Session#session{keepalive_timer = KeepAlive}};

open_confirm(tcp_closed, Session) ->
  error_logger:info_msg("FSM:open_confirm/tcp_closed~n"),
  {stop, normal, Session};

open_confirm(tcp_fatal, Session) ->
  error_logger:info_msg("FSM:open_confirm/tcp_fatal~n"),
  {stop, normal, Session};

open_confirm(_Event, Session) ->
  error_logger:info_msg("FSM:open_confirm/other(~p)~n", [_Event]),
  send_notification(Session, ?BGP_ERR_FSM),
  {stop, normal, Session}.


% Established state.

established(start, Session) ->
  error_logger:info_msg("FSM:established/start~n"),
  {next_state, established, Session};

established(stop, Session) ->
  error_logger:info_msg("FSM:established/stop~n"),
  send_notification(Session, ?BGP_ERR_CEASE),
  {stop, stop, Session};

established({update_received, Bin, Len},
            #session{local_asn = LocalASN} = Session) ->
  error_logger:info_msg("FSM:established/update_received~n"),
  Hold = restart_timer(hold, Session),
  NewSession = Session#session{hold_timer = Hold},
  case rtm_parser:parse_update(Bin, Len, LocalASN) of
    {ok, Msg} ->
      update_rib(Session, Msg),
      {next_state, established, NewSession};
    {error, Error} ->
      log_error(Error),
      send_notification(NewSession, ?BGP_ERR_UPDATE),
      {stop, normal, NewSession}
  end;

established(keepalive_received, Session) ->
  error_logger:info_msg("FSM:established/keepalive_received~n"),
  Hold = restart_timer(hold, Session),
  {next_state, established, Session#session{hold_timer = Hold}};

established({notification_received, Bin}, Session) ->
  error_logger:info_msg("FSM:established/notification_received~n"),
  log_notification(Bin),
  {stop, normal, Session};

established({timeout, _Ref, hold}, Session) ->
  error_logger:info_msg("FSM:established/hold_timeout~n"),
  send_notification(Session, ?BGP_ERR_HOLD_TIME),
  {stop, normal, Session};

established({timeout, _Ref, keepalive}, Session) ->
  error_logger:info_msg("FSM:established/keepalive_timeout~n"),
  KeepAlive = restart_timer(keepalive, Session),
  send_keepalive(Session),
  {next_state, established, Session#session{keepalive_timer = KeepAlive}};

established(tcp_closed, Session) ->
  error_logger:info_msg("FSM:established/tcp_closed~n"),
  {stop, normal, Session};

established(tcp_fatal, Session) ->
  error_logger:info_msg("FSM:established/tcp_fatal~n"),
  {stop, normal, Session};

established(_Event, Session) ->
  error_logger:info_msg("FSM:established/other(~p)~n", [_Event]),
  send_notification(Session, ?BGP_ERR_FSM),
  {stop, normal, Session}.


%
% gen_fsm callbacks.
%

terminate(_Reason, _StateName, Session) ->
  release_resources(Session),
  ok.

code_change(_OldVsn, StateName, Session, _Extra) ->
  {ok, StateName, Session}.

handle_event(_Event, _StateName, Session) ->
  {stop, unexpected_event, Session}.

handle_sync_event(state, _From, StateName, Session) ->
  {reply, StateName, StateName, Session}.

handle_info(_Info, _StateName, Session) ->
  {stop, unexpected_info, Session}.

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

% Expects Time to be in seconds.
start_timer(Name, Time) ->
  gen_fsm:start_timer(timer:seconds(Time), Name).

clear_timer(undefined) ->
  false;
clear_timer(Timer) ->
  gen_fsm:cancel_timer(Timer).

connect_to_peer(#session{server     = undefined,
                         local_addr = LocalAddr,
                         peer_addr  = PeerAddr} = Session) ->
  {ok, Pid} = rtm_server_sup:start_child(self()),
  SockOpts = [binary, {ip, LocalAddr}, {packet, raw}, {active, false}],
  case gen_tcp:connect(PeerAddr, ?BGP_PORT, SockOpts) of
    {ok, Socket} ->
      rtm_server:set_socket(Pid, Socket),
      inet:setopts(Socket, [{active, once}]),
      gen_tcp:controlling_process(Socket, Pid),
      gen_fsm:send_event(self(), tcp_open),
      Session#session{server = Pid};
    {error, _Reason} ->
      gen_fsm:send_event(self(), tcp_open_failed),
      Session
  end.

close_connection(#session{server = Server} = Session) ->
  rtm_server:close_peer_connection(Server),
  Session#session{server = undefined}.

release_resources(Session) ->
  NewSession = close_connection(Session),
  Deleted = rtm_rib:remove_prefixes(),
  send_updates(Session, peers(), dict:new(), [], Deleted),
  clear_timer(Session#session.conn_retry_timer),
  clear_timer(Session#session.hold_timer),
  clear_timer(Session#session.keepalive_timer),
  NewSession.

check_peer(#session{server = Server, peer_addr = PeerAddr}) ->
  case rtm_server:peer_addr(Server) of
    {ok, PeerAddr} -> ok;
    {ok, _}          -> bad_peer
  end.

negotiate_hold_time(LocalHoldTime, PeerHoldTime) ->
  HoldTime = min(LocalHoldTime, PeerHoldTime),
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

log_notification(Bin) ->
  {ok, #bgp_notification{error_string = Err}} =
    rtm_parser:parse_notification(Bin),
  error_logger:info_msg("~s~n", [Err]).

log_error({Code, SubCode, Data}) ->
  error_logger:error_msg(rtm_util:error_string(Code, SubCode, Data)).

%
% TODO Move the stuff below to a separate simple-one-for-one rtm_rdp
% gen_server process so that the FSM doesn't need to block and the RIB
% can be asynchronous.
%

update_rib(#session{peer_bgp_id = PeerBGPId,
                    peer_addr   = PeerAddr} = Session,
           #bgp_update{path_attrs       = PathAttrs,
                       withdrawn_routes = Withdrawn,
                       nlri             = NLRI}) ->
  Route = #route{
    active     = false,
    next_hop   = rtm_attr:get(?BGP_PATH_ATTR_NEXT_HOP, PathAttrs),
    path_attrs = PathAttrs,
    ebgp       = is_ebgp(Session),
    bgp_id     = PeerBGPId,
    peer_addr  = PeerAddr,
    fsm        = self()
  },
  {Added, Deleted, Replacements} = rtm_rib:update(Route, NLRI, Withdrawn),
  redistribute_routes(Session, PathAttrs, Added, Deleted, Replacements).

redistribute_routes(#session{server = Originator} = Session, Attrs,
                    Added, Deleted, Replacements) ->
  AllServers = case is_ebgp(Session) of
    true  -> peers();
    false -> ebgp_peers()
  end,
  Servers = lists:filter(fun(Server) -> Server =/= Originator end, AllServers),
  send_updates(Session, Servers, Attrs, Added, Deleted),
  send_updates(Session, Servers, Replacements).

distribute_installed_routes(#session{server = Server} = Session) ->
  send_updates(Session, [Server], rtm_rib:get()).

is_ebgp(#session{local_asn = LocalASN, peer_asn = PeerASN}) ->
  LocalASN =/= PeerASN.

join_established_group(#session{server = Server} = Session) ->
  ok = pg2:join(established_group(Session), Server).

established_group(Session) ->
  case is_ebgp(Session) of
    true  -> established_ebgp;
    false -> established_ibgp
  end.

peers() ->
  ebgp_peers() ++ ibgp_peers().

ebgp_peers() ->
  pg2:get_members(established_ebgp).

ibgp_peers() ->
  pg2:get_members(established_ibgp).


% Message sending.

send_open(#session{server     = Server,
                   local_asn  = ASN,
                   hold_time  = HoldTime,
                   local_addr = LocalAddr}) ->
  Msg = rtm_msg:open(ASN, HoldTime, LocalAddr),
  send(Server, Msg).

send_updates(Session, Servers, Attrs, Added, Deleted) ->
  AttrsToSend = update_path_attrs(Session, Attrs),
  lists:foreach(fun(Server) ->
    send_update(Server, AttrsToSend, Added, Deleted)
  end, Servers).

send_updates(Session, Servers, Routes) ->
  dict:fold(fun(#route{path_attrs = Attrs}, Prefixes, ok) ->
    AttrsToSend = update_path_attrs(Session, Attrs),
    lists:foreach(fun(Server) ->
      send_update(Server, AttrsToSend, Prefixes, [])
    end, Servers)
  end, ok, Routes).

send_update(Server, PathAttrs, Added, Deleted) ->
  Msg = rtm_msg:update(PathAttrs, Added, Deleted),
  send(Server, Msg).

send_notification(#session{server = Server}, Error) ->
  send(Server, rtm_msg:notification(Error)).

send_keepalive(#session{server = Server}) ->
  send(Server, rtm_msg:keepalive()).

send(Server, Bin) ->
  rtm_server:send_msg(Server, Bin).

% ORIGIN: always keep
% AS_PATH: update if propagating to external peer or when originating
% NEXT_HOP: update if propagating to external peer
% MED: propagate only to internal peers
% LOCAL_PREF: propagate only to internal peers
% ATOMIC_AGGREGATE: always propagate
% TODO AGGREGATOR: propagate if doing aggregation
update_path_attrs(#session{local_asn  = LocalASN,
                           local_addr = LocalAddr} = Session,
                  PathAttrs) ->
  UpdateAttrs = case established_group(Session) of
    established_ebgp ->
      fun(TypeCode, Attr, Attrs) ->
        rtm_attr:update_for_ebgp(TypeCode, Attr, Attrs, LocalASN, LocalAddr)
      end;
    established_ibgp ->
      fun rtm_attr:update_for_ibgp/3
  end,
  rtm_attr:fold(UpdateAttrs, PathAttrs, PathAttrs).
