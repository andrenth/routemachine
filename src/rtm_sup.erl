-module(rtm_sup).

-export([start_link/1]).
-export([init/1]).

start_link(ListenPort) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ListenPort).

init(ListenPort) ->
  AcceptorSupSpec =
    {rtm_acceptor_sup,
      {rtm_acceptor_sup, start_link, [ListenPort]},
      permanent,
      infinity,
      supervisor,
      [rtm_acceptor_sup]},
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
  {ok, {{one_for_one, 1, 1}, [AcceptorSupSpec, ServerSupSpec, FsmSupSpec]}}.
