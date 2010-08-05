-module(rtm_acceptor_sup).
-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
  SockOpts = [binary, {reuseaddr, true}, {packet, raw}, {active, false}],
  {ok, ListenSocket} = gen_tcp:listen(Port, SockOpts),
  AcceptorSpec =
    {rtm_acceptor,
      {rtm_acceptor, start_link, [ListenSocket]},
      permanent,
      2000,
      worker,
      [rtm_acceptor]},
  {ok, {{one_for_one, 1, 1}, [AcceptorSpec]}}.
