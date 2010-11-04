-module(rtm_sup).

-include_lib("routemachine.hrl").
-include_lib("session.hrl").

-export([start_link/2]).
-export([init/1]).

start_link(Config, Sessions) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, {Config, Sessions}).

init({Config, Sessions}) ->
  ListenPort = rtm_config:get(listen_port, Config, ?DEFAULT_PORT),
  Networks = rtm_config:networks(Config),
  SockOpts = [binary, {reuseaddr, true}, {packet, raw}, {active, false}],
  {ok, ListenSocket} = gen_tcp:listen(ListenPort, SockOpts),
  ActiveSessions = dict:filter(fun is_active/2, Sessions),
  pg2:create(updaters),

  ChildSpecs = [
    {rtm_rib,
      {rtm_rib, start_link, []},
      permanent,
      2000,
      worker,
      [rtm_rib]},

    {rtm_server_sup,
      {rtm_server_sup, start_link, []},
      permanent,
      infinity,
      supervisor,
      [rtm_server_sup]},

    {rtm_fsm_sup,
      {rtm_fsm_sup, start_link, [ActiveSessions]},
      permanent,
      infinity,
      supervisor,
      [rtm_fsm_sup]},

    {rtm_updater_sup,
      {rtm_updater_sup, start_link, []},
      permanent,
      infinity,
      supervisor,
      [rtm_updater_sup]},

    {rtm_acceptor,
      {rtm_acceptor, start_link, [ListenSocket, Sessions]},
      permanent,
      brutal_kill,
      worker,
      [rtm_acceptor]},

    {rtm_watcher,
      {rtm_watcher, start_link, [Networks]},
      permanent,
      brutal_kill,
      worker,
      [rtm_watcher]}
  ],

  {ok, {{one_for_one, 1, 1}, ChildSpecs}}.

is_active(_IP, #session{establishment = active}) -> true;
is_active(_IP, #session{establishment = {passive, _Socket}}) -> false.
