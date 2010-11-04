-module(rtm_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("bgp.hrl").
-include_lib("session.hrl").

start(_Type, _Args) ->
  Config = rtm_config:parse("routemachine/routemachine.conf"),
  PeerList = rtm_config:peers(Config),
  Sessions = lists:foldl(fun(#session{peer_addr = Ip} = Session, Acc) ->
    dict:store(Ip, Session, Acc)
  end, dict:new(), PeerList),
  rtm_sup:start_link(Config, Sessions).

stop(_State) ->
  ok.
