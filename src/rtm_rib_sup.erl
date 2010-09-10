-module(rtm_rib_sup).
-behavior(supervisor).

-export([start_link/0, start_child/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ok).

start_child() ->
  supervisor:start_child(?MODULE, []).

init(ok) ->
  RibSpec =
    {rtm_rib,
      {rtm_rib, start_link, []},
      temporary,
      brutal_kill,
      worker,
      [rtm_rib]},
  {ok, {{simple_one_for_one, 0, 1}, [RibSpec]}}.
