-module(rtm_fsm_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
  FsmSpec =
    {rtm_fsm,
      {rtm_fsm, start_link, [Port]},
      permanent,
      2000,
      worker,
      [rtm_fsm]},
  {ok, {{one_for_one, 1, 1}, [FsmSpec]}}.
