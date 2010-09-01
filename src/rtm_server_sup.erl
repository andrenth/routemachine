-module(rtm_server_sup).
-behavior(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ok).

start_child(FSM) ->
  supervisor:start_child(?MODULE, [FSM]).

init(ok) ->
  ServerSpec =
    {rtm_server,
      {rtm_server, start_link, []},
      temporary,
      brutal_kill,
      worker,
      [rtm_server]},
  {ok, {{simple_one_for_one, 0, 1}, [ServerSpec]}}.
