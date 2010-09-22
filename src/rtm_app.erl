-module(rtm_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("bgp.hrl").
-include_lib("session.hrl").

-define(DEFAULT_PORT, 1179).

start(_Type, _Args) ->
  Config = rtm_config:parse("routemachine/routemachine.conf"),
  ListenPort = rtm_config:get(listen_port, Config, ?DEFAULT_PORT),
  PeerList = rtm_config:peers(Config),
  Peers = lists:foldl(fun(#session{peer_addr = Ip} = Session, Acc) ->
    dict:store(Ip, Session, Acc)
  end, dict:new(), PeerList),
  rtm_sup:start_link(ListenPort, Peers).

stop(_State) ->
  ok.
