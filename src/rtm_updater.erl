-module(rtm_updater).
-behaviour(gen_server).

-export([start_link/1]).

% API
-export([redistribute_routes/2, distribute_installed_routes/1,
         remove_prefixes/1, added_local_route/3, deleted_local_route/3]).

% Exports for gen_server.
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2,
         code_change/3]).

-include_lib("bgp.hrl").
-include_lib("route.hrl").
-include_lib("session.hrl").

-spec start_link(#session{}) -> term().
start_link(Session) ->
  gen_server:start_link(?MODULE, Session, []).

%
% API
%

-spec redistribute_routes(pid(), #bgp_update{}) -> ok.
redistribute_routes(Updater, Msg) ->
  gen_server:cast(Updater, {redistribute_routes, Msg}).

-spec distribute_installed_routes(pid()) -> ok.
distribute_installed_routes(Updater) ->
  gen_server:cast(Updater, distribute_installed_routes).

-spec remove_prefixes(pid()) -> ok.
remove_prefixes(Updater) ->
  gen_server:cast(Updater, remove_prefixes).

-spec added_local_route(pid(), [prefix()], dict()) -> ok.
added_local_route(Updater, Prefixes, PathAttrs) ->
  gen_server:cast(Updater, {added_local_route, Prefixes, PathAttrs}).

-spec deleted_local_route(pid(), {[prefix()], dict()}, dict()) -> ok.
deleted_local_route(Updater, Update, PathAttrs) ->
  gen_server:cast(Updater, {deleted_local_route, Update, PathAttrs}).

%
% Callbacks for gen_server.
%

init(Session) ->
  process_flag(trap_exit, true),
  pg2:join(updaters, self()),
  {ok, Session}.

handle_cast({redistribute_routes, #bgp_update{path_attrs       = PathAttrs,
                                              withdrawn_routes = Withdrawn,
                                              nlri             = Nlri}},
            #session{local_asn   = LocalAsn,
                     peer_addr   = PeerAddr,
                     peer_bgp_id = PeerId} = Session) ->
  AsPath = rtm_attr:get(?BGP_PATH_ATTR_AS_PATH, PathAttrs),
  RouteAttrs = #route_attrs{
    active       = false,
    next_hop     = rtm_attr:get(?BGP_PATH_ATTR_NEXT_HOP, PathAttrs),
    path_attrs   = PathAttrs,
    ebgp         = is_ebgp(Session),
    as_path_loop = as_path_has_loop(AsPath, LocalAsn),
    peer_bgp_id  = PeerId,
    peer_addr    = PeerAddr
  },
  {Added, Replaced, Deleted, Replacements} = rtm_rib:update(RouteAttrs,
                                                            Nlri, Withdrawn),
  send_replaced_routes(Session, Replaced),
  send_updates(Session, PathAttrs, {Added, Deleted, Replacements}),
  {noreply, Session};

handle_cast(distribute_installed_routes, #session{server = Server} = Session) ->
  rtm_msg:send_updates(Session, [Server], rtm_rib:best_routes()),
  {noreply, Session};

handle_cast({added_local_route, Added, PathAttrs}, Session) ->
  send_updates(Session, PathAttrs, {Added, [], dict:new()}),
  {noreply, Session};

handle_cast({deleted_local_route, {Deleted, Repls}, PathAttrs}, Session) ->
  send_updates(Session, PathAttrs, {[], Deleted, Repls}),
  {noreply, Session};

handle_cast(remove_prefixes, #session{peer_bgp_id = PeerId} = Session) ->
  Deleted = rtm_rib:remove_prefixes(PeerId),
  rtm_msg:send_updates(Session, bgp_peers(), dict:new(), [], Deleted),
  {stop, normal, Session}.

handle_call(_Req, _From, Session) ->
  {stop, unexpected_call, Session}.

handle_info(_Info, Session) ->
  {stop, unexpected_info, Session}.

terminate(_Reason, _Session) ->
  ok.

code_change(_OldVsn, ok, _Extra) ->
  {ok, ok}.

%
% Internal functions.
%

send_replaced_routes(Session, Replaced) ->
  Servers = servers(Session),
  lists:foreach(fun({Prefix, Attrs}) ->
    rtm_msg:send_updates(Session, Servers, Attrs, [], [Prefix])
  end, Replaced).

send_updates(Session, Attrs, {Added, Deleted, Replacements}) ->
  Servers = servers(Session),
  rtm_msg:send_updates(Session, Servers, Attrs, Added, Deleted),
  rtm_msg:send_updates(Session, Servers, Replacements).

servers(#session{server = Originator} = Session) ->
  Servers = case is_ebgp(Session) of
    true  -> bgp_peers();
    false -> ebgp_peers()
  end,
  lists:filter(fun(Server) -> Server =/= Originator end, Servers).

-spec as_path_has_loop([{?BGP_AS_PATH_SET | ?BGP_AS_PATH_SEQUENCE, uint16()}],
                       uint16()) -> boolean().
as_path_has_loop([], _LocalAsn) ->
  false;
as_path_has_loop([{_Type, Asns} | Rest], LocalAsn) ->
  case lists:member(LocalAsn, Asns) of
    true  -> true;
    false -> as_path_has_loop(Rest, LocalAsn)
  end.

is_ebgp(#session{local_asn = LocalAsn, peer_asn = PeerAsn}) ->
  LocalAsn =/= PeerAsn.

bgp_peers() ->
  ebgp_peers() ++ ibgp_peers().

ebgp_peers() ->
  pg2:get_members(established_ebgp).

ibgp_peers() ->
  pg2:get_members(established_ibgp).
