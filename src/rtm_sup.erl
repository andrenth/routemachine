-module(rtm_sup).

-export([start_link/2]).
-export([init/1]).

start_link(ListenPort, Peers) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, {ListenPort, Peers}).

init({ListenPort, Peers}) ->
  SockOpts = [binary, {reuseaddr, true}, {packet, raw}, {active, false}],
  {ok, ListenSocket} = gen_tcp:listen(ListenPort, SockOpts),

  AcceptorSpec =
    {rtm_acceptor,
      {rtm_acceptor, start_link, [ListenSocket, Peers]},
      permanent,
      2000,
      worker,
      [rtm_acceptor]},

  ServerSupSpec =
    {rtm_server_sup,
      {rtm_server_sup, start_link, []},
      permanent,
      infinity,
      supervisor,
      [rtm_server_sup]},

  FsmSupSpec =
    {rtm_fsm_sup,
      {rtm_fsm_sup, start_link, []},
      permanent,
      infinity,
      supervisor,
      [rtm_fsm_sup]},

  {ok, {{one_for_one, 1, 1}, [AcceptorSpec, ServerSupSpec, FsmSupSpec]}}.
