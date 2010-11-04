-module(rtm_updater_sup).
-behavior(supervisor).

-export([start_link/0, start_child/1, terminate_child/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ok).

start_child(Session) ->
  supervisor:start_child(?MODULE, [Session]).

terminate_child(Pid) ->
  supervisor:terminate_child(?MODULE, Pid).

init(ok) ->
  UpdaterSpec =
    {rtm_updater,
      {rtm_updater, start_link, []},
      temporary,
      brutal_kill,
      worker,
      [rtm_updater]},
  {ok, {{simple_one_for_one, 0, 1}, [UpdaterSpec]}}.
