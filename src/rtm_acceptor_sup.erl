-module(rtm_acceptor_sup).
-behavior(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ok).

start_child(ListenPort, FsmPid) ->
  {ok, Pid} = supervisor:start_child(?MODULE, [ListenPort, FsmPid]),
  io:format("Starting new acceptor child ~w~n", [Pid]),
  {ok, Pid}.

init(ok) ->
  AcceptorSpec =
    {rtm_acceptor,
      {rtm_acceptor, start_link, []},
      permanent,
      2000,
      worker,
      [rtm_acceptor]},
  {ok, {{simple_one_for_one, 0, 1}, [AcceptorSpec]}}.
