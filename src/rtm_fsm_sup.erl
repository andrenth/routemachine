-module(rtm_fsm_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ok).

start_child(Mode) ->
  supervisor:start_child(?MODULE, [Mode]).

init(ok) ->
  FsmSpec =
    {rtm_fsm,
      {rtm_fsm, start_link, []},
      temporary,
      brutal_kill,
      worker,
      [rtm_fsm]},
  {ok, {{simple_one_for_one, 0, 1}, [FsmSpec]}}.
