-module(rtm_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("bgp.hrl").
-include_lib("session.hrl").

-define(DEFAULT_PORT, 1179).

start(_Type, _Args) ->
  ListenPort =
    case application:get_env(routemachine, listen_port) of
      {ok, ConfigPort} -> ConfigPort;
      undefined        -> ?DEFAULT_PORT
    end,
  PeerList = [
    #session{
      establishment   = passive,
      local_asn       = 2,
      remote_asn      = 1,
      local_addr      = {10,7,5,4},
      remote_addr     = {10,7,5,167},
      hold_time       = ?BGP_TIMER_HOLD,
      keepalive_time  = ?BGP_TIMER_KEEPALIVE,
      conn_retry_time = ?BGP_TIMER_CONN_RETRY
    }
  ],
  Peers = lists:foldl(fun(#session{remote_addr = Ip} = Session, Acc) ->
    dict:store(Ip, Session, Acc)
  end, dict:new(), PeerList),
  rtm_sup:start_link(ListenPort, Peers).

stop(_State) ->
  ok.
