-module(rtm_sup).

-include_lib("session.hrl").

-export([start_link/2]).
-export([init/1]).

start_link(ListenPort, Sessions) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, {ListenPort, Sessions}).

init({ListenPort, Sessions}) ->
  SockOpts = [binary, {reuseaddr, true}, {packet, raw}, {active, false}],
  {ok, ListenSocket} = gen_tcp:listen(ListenPort, SockOpts),
  ActiveSessions = dict:filter(fun is_active/2, Sessions),

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

    {rtm_main,
      {rtm_main, start_link, [ListenSocket, Sessions]},
      permanent,
      brutal_kill,
      worker,
      [rtm_main]}
  ],

  {ok, {{one_for_one, 1, 1}, ChildSpecs}}.

is_active(_IP, #session{establishment = active}) -> true;
is_active(_IP, #session{establishment = passive}) -> false.
