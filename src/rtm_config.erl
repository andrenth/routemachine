-module(rtm_config).
-export([parse/1, peers/1, get/2, get/3]).

-include_lib("bgp.hrl").
-include_lib("session.hrl").

parse(File) ->
  {ok, Conf} = file:consult(File),
  Conf.

peers(Conf) ->
  {local, Local} = get(local, Conf),
  Peers = get_all(peer, Conf),
  build_session(Local, Peers).

get(Key, Conf) ->
  proplists:lookup(Key, Conf).

get(Key, Conf, Default) ->
  proplists:get_value(Key, Conf, Default).

get_all(Key, Conf) ->
  proplists:lookup_all(Key, Conf).

build_session(Local, Peers) ->
  build_session(Local, Peers, []).

build_session(_Local, [], Sessions) ->
  Sessions;
build_session(Local, [{peer, Peer} | Rest], Sessions) ->
  {asn, LocalAsn}      = get(asn, Local),
  {address, LocalAddr} = get(address, Local),
  {asn, PeerAsn}       = get(asn, Peer),
  {address, PeerAddr}  = get(address, Peer),
  Session = #session{
    local_asn       = LocalAsn,
    local_addr      = LocalAddr,
    peer_asn        = PeerAsn,
    peer_addr       = PeerAddr,
    hold_time       = get(hold_time, Peer, ?BGP_TIMER_HOLD),
    keepalive_time  = get(keepalive_time, Peer, ?BGP_TIMER_KEEPALIVE),
    conn_retry_time = get(conn_retry_time, Peer, ?BGP_TIMER_CONN_RETRY),
    establishment   = get(establishment, Peer, active)
  },
  build_session(Local, Rest, [Session | Sessions]).
