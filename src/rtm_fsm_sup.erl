-module(rtm_fsm_sup).
-behaviour(supervisor).

-include_lib("session.hrl").

-export([start_link/1, start_child/1, delete_child/1]).
-export([init/1]).

start_link(ActiveSessions) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ActiveSessions).

start_child(Session) ->
  Spec = build_spec(Session),
  supervisor:start_child(?MODULE, Spec).

delete_child(Id) ->
  supervisor:delete_child(?MODULE, Id).

init(ActiveSessions) ->
  FsmSpecs = dict:fold(fun(_IP, Session, Specs) ->
    [build_spec(Session) | Specs]
  end, [], ActiveSessions),
  {ok, {{one_for_one, 1, 1}, FsmSpecs}}.

build_spec(#session{peer_addr = PeerAddr} = Session) ->
  {{rtm_fsm, PeerAddr},
    {rtm_fsm, start_link, [Session]},
    temporary,
    2000,
    worker,
    [rtm_fsm]}.
