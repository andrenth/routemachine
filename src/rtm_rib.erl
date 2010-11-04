-module(rtm_rib).
-behaviour(gen_server).

-include_lib("bgp.hrl").
-include_lib("route.hrl").

-export([start_link/0]).

% API.
-export([best_routes/0, add/2, del/2, update/3, remove_prefixes/1]).

% Exports for gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {
  rib :: rib()
}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, ok, []).

%
% API.
%

-spec best_routes() -> dict().
best_routes() ->
  gen_server:call(?MODULE, best_routes).

-spec update(#route_attrs{}, [prefix()], [prefix()]) ->
        {[prefix()], [prefix()], rib()}.
update(RouteAttrs, Nlri, Withdrawn) ->
  gen_server:call(?MODULE, {update, RouteAttrs, Nlri, Withdrawn}).

-spec remove_prefixes(uint32()) -> [prefix()].
remove_prefixes(PeerBgpId) ->
  gen_server:call(?MODULE, {remove_prefixes, PeerBgpId}).

-spec add(#route_attrs{}, [prefix()]) -> [prefix()].
add(RouteAttrs, Prefixes) ->
  gen_server:call(?MODULE, {add, RouteAttrs, Prefixes}).

-spec del(#route_attrs{}, [prefix()]) -> {[prefix()], dict()}.
del(RouteAttrs, Prefixes) ->
  gen_server:call(?MODULE, {del, RouteAttrs, Prefixes}).

%
% Callbacks for gen_server.
%

init(ok) ->
  error_logger:info_msg("Starting RIB server ~w~n", [self()]),
  process_flag(trap_exit, true),
  register(?MODULE, self()),
  {ok, #state{rib = dict:new()}}.

handle_call(best_routes, _From, #state{rib = Rib} = State) ->
  RouteAttrs = dict:fold(fun(Prefix, RouteAttrsList, Best) ->
    case RouteAttrsList of
      [BestAttrs | _Rest] -> dict:append(BestAttrs, Prefix, Best);
      [] -> Best
    end
  end, dict:new(), Rib),
  {reply, RouteAttrs, State};

handle_call({add, RouteAttrs, Prefixes}, _From, #state{rib = Rib} = State) ->
  {Added, NewRib} = add_prefixes(Rib, RouteAttrs, Prefixes),
  AddedPrefixes = lists:map(fun(T) -> element(2, T) end, Added),
  {reply, AddedPrefixes, State#state{rib = NewRib}};

handle_call({del, #route_attrs{peer_bgp_id = PeerBgpId}, Prefixes}, _From,
            #state{rib = Rib} = State) ->
  {Deleted, Replacements, NewRib} = del_prefixes(Rib, Prefixes, PeerBgpId),
  {reply, {Deleted, Replacements}, State#state{rib = NewRib}};

handle_call({update, #route_attrs{peer_bgp_id = PeerBgpId,
                                  next_hop    = NextHop} = RouteAttrs,
             Nlri, Withdrawn}, _From, #state{rib = Rib} = State) ->
  {Added, TmpRib} = add_prefixes(Rib, RouteAttrs, Nlri),
  {Deleted, Replacements, NewRib} = del_prefixes(TmpRib, Withdrawn, PeerBgpId),
  add_routes(Added, NextHop),
  del_routes(Deleted, NextHop),
  add_replacements(Replacements),
  AddedPrefixes = lists:map(fun(T) -> element(2, T) end, Added),
  {reply, {AddedPrefixes, Deleted, Replacements}, State#state{rib = NewRib}};

handle_call({remove_prefixes, PeerBgpId}, _From, #state{rib = Rib} = State) ->
  {Removed, NewRib} = lists:foldl(fun(Prefix, {AccRemoved, AccRib}) ->
    case del_prefixes(AccRib, [Prefix], PeerBgpId) of
      {[Deleted], _, NewAccRib} -> {[Deleted | AccRemoved], NewAccRib};
      {[], _, NewAccRib}        -> {AccRemoved, NewAccRib}
    end
  end, {[], Rib}, dict:fetch_keys(Rib)),
  {reply, Removed, State#state{rib = NewRib}}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_cast(_Request, State) ->
  {stop, unexpected_cast, State}.

handle_info({_Port, {data, _Data}}, State) ->
  {noreply, State};

handle_info({'EXIT', _Port, normal}, State) ->
  {noreply, State};

handle_info({'EXIT', _Port, Error}, State) ->
  error_logger:error_msg("RTM port died with error ~p~n", [Error]),
  {noreply, State}.

terminate(_Reason, #state{rib = Rib}) ->
  clear_loc_rib(Rib),
  ok.

%
% Internal functions.
%

-spec del_prefixes(rib(), [prefix()], ipv4_address()) ->
        {[prefix()], dict(), rib()}.
del_prefixes(Rib, Prefixes, PeerBgpId) ->
  lists:foldl(fun(Prefix, {Deleted, Replacements, AccRib}) ->
    case remove_entry(Prefix, AccRib, PeerBgpId) of
      {inactive, NewAccRib} ->
        {Deleted, Replacements, NewAccRib};
      {replaced, RouteAttrs, NewAccRib} ->
        {Deleted, dict:append(RouteAttrs, Prefix, Replacements), NewAccRib};
      {unfeasible, Prefix, NewAccRib} ->
        {[Prefix | Deleted], Replacements, NewAccRib}
    end
  end, {[], dict:new(), Rib}, Prefixes).

-spec remove_entry(prefix(), rib(), ipv4_address()) ->
        {replaced, #route_attrs{}, rib()} | {unfeasible, prefix(), rib()} |
        {inactive, rib()}.
remove_entry(Prefix, Rib, PeerBgpId) ->
  case dict:find(Prefix, Rib) of
    {ok, RouteAttrsList} ->
      FromId = fun(#route_attrs{peer_bgp_id = Id}) -> Id =:= PeerBgpId end,
      {Removed, Remaining} = lists:partition(FromId, RouteAttrsList),
      NewRib = dict:store(Prefix, Remaining, Rib),
      case removed_active(Removed) of
        true ->
          case Remaining of
            [] ->
              {unfeasible, Prefix, dict:erase(Prefix, NewRib)};
            [#route_attrs{active = false} = RouteAttrs | _] ->
              {replaced, RouteAttrs, NewRib}
          end;
        false ->
          {inactive, NewRib}
      end;
    error ->
      error_logger:error_msg("Tried to remove nonexistant route~n"),
      {inactive, Rib}
  end.

-spec removed_active([#route_attrs{}]) -> boolean().
removed_active([#route_attrs{active = true} | _Rest]) ->
  true;
removed_active(_RouteAttrsList) ->
  false.

-spec add_prefixes(rib(), #route_attrs{}, [prefix()]) ->
        {[{added, prefix()} | {replacement, prefix(), uint32()}], rib()}.
add_prefixes(Rib, RouteAttrs, Prefixes) ->
  InsertPrefix = fun(Prefix, {Added, AccRib}) ->
    case dict:find(Prefix, AccRib) of
      {ok, RouteAttrsList} ->
        case insert_route(RouteAttrs, RouteAttrsList) of
          {new, NewRouteAttrsList} ->
            NewRib = dict:store(Prefix, NewRouteAttrsList, AccRib),
            {[{added, Prefix} | Added], NewRib};
          {replacement, NewRouteAttrsList} ->
            NewRib = dict:store(Prefix, NewRouteAttrsList, AccRib),
            [_, #route_attrs{next_hop = OldNextHop} | _] = NewRouteAttrsList,
            {[{replacement, Prefix, OldNextHop} | Added], NewRib};
          {inactive, NewRouteAttrsList} ->
            NewRib = dict:store(Prefix, NewRouteAttrsList, AccRib),
            {Added, NewRib};
          equal ->
            {Added, Rib}
        end;
      error ->
        NewRouteAttrs = RouteAttrs#route_attrs{active = true},
        NewRib = dict:store(Prefix, [NewRouteAttrs], AccRib),
        {[{added, Prefix} | Added], NewRib}
    end
  end,
  lists:foldl(InsertPrefix, {[], Rib}, Prefixes).

-spec insert_route(#route_attrs{}, [#route_attrs{}]) ->
        {new | replacement | inactive, [#route_attrs{}]} | equal.
insert_route(RouteAttrs, RouteAttrsList) ->
  insert_route(RouteAttrs, RouteAttrsList, true).

insert_route(NewRouteAttrs, [], false) ->
  {inactive, [NewRouteAttrs]};
insert_route(NewRouteAttrs, [], true) ->
  {new, [NewRouteAttrs#route_attrs{active = true}]};
insert_route(NewRouteAttrs, [RouteAttrs | Rest], First) ->
  case preference_cmp(NewRouteAttrs, RouteAttrs) of
    gt ->
      ActRouteAttrs  = NewRouteAttrs#route_attrs{active = First},
      InactRoutAttrs = RouteAttrs#route_attrs{active = false},
      {replacement, [ActRouteAttrs, InactRoutAttrs | Rest]};
    lt ->
      {inactive, NewRest} = insert_route(NewRouteAttrs, Rest, false),
      {inactive, [RouteAttrs | NewRest]};
    eq ->
      equal
  end.

clear_loc_rib(Rib) ->
  lists:foreach(fun({Prefix, RouteAttrsList}) ->
    lists:foreach(fun(#route_attrs{next_hop = NextHop}) ->
      del_route(Prefix, NextHop)
    end, RouteAttrsList)
  end, dict:to_list(Rib)).

add_routes([], _NextHop) ->
  ok;
add_routes([{added, Prefix} | Rest], NextHop) ->
  add_route(Prefix, NextHop),
  add_routes(Rest, NextHop);
add_routes([{replacement, Prefix, OldNextHop} | Rest], NextHop) ->
  del_route(Prefix, OldNextHop),
  add_route(Prefix, NextHop),
  add_routes(Rest, NextHop).

add_replacements(Replacements) ->
  dict:fold(fun(#route_attrs{next_hop = NextHop}, Prefix, ok) ->
    add_route(Prefix, NextHop)
  end, ok, Replacements).

del_routes([], _NextHop) ->
  ok;
del_routes([Prefix | Rest], NextHop) ->
  del_route(Prefix, NextHop),
  del_routes(Rest, NextHop).

add_route(Prefix, NextHop) ->
  io:format("Adding route: ~p via ~p~n", [Prefix, NextHop]),
  modify_route("add", Prefix, NextHop).

del_route(Prefix, NextHop) ->
  io:format("Deleting route: ~p via ~p~n", [Prefix, NextHop]),
  modify_route("del", Prefix, NextHop).

modify_route(Cmd, {Pref, Len}, NextHop) ->
  Dst = Pref bsl (32 - Len),
  run_rtm(Cmd, Len, Dst, NextHop).

run_rtm(Cmd, Len, Dst, GW) ->
  Args = [Cmd, integer_to_list(Len), integer_to_list(Dst), integer_to_list(GW)],
  PrivDir = code:priv_dir(routemachine),
  Port = open_port({spawn_executable, filename:join([PrivDir, "rtm"])},
                   [stream, {args, Args}]),
  erlang:port_close(Port).

preference_cmp(#route_attrs{active = false} = RouteAttrs1,
               #route_attrs{active = true}  = RouteAttrs2) ->
  % TODO check reachability
  Attributes = [
    fun local_pref/2,
    fun as_path_len/2,
    fun origin/2,
    fun med/2,
    fun ebgp/2,
    %fun weight/2,
    fun bgp_id/2,
    fun peer_addr/2
  ],
  compare_in_order(Attributes, RouteAttrs1, RouteAttrs2).

compare_in_order([], _RouteAttrs1, _RouteAttrs2) ->
  eq;
compare_in_order([Fun | Rest], RouteAttrs1, RouteAttrs2) ->
  case Fun(RouteAttrs1, RouteAttrs2) of
    X when X > 0 -> gt;
    X when X < 0 -> lt;
    0 -> compare_in_order(Rest, RouteAttrs1, RouteAttrs2)
  end.

local_pref(RouteAttrs1, RouteAttrs2) ->
  sub_attrs(?BGP_PATH_ATTR_LOCAL_PREF, RouteAttrs1, RouteAttrs2).

as_path_len(RouteAttrs1, RouteAttrs2) ->
  ASPath1 = attr_value(?BGP_PATH_ATTR_AS_PATH, RouteAttrs1),
  ASPath2 = attr_value(?BGP_PATH_ATTR_AS_PATH, RouteAttrs2),
  byte_size(ASPath2) - byte_size(ASPath1).

origin(RouteAttrs1, RouteAttrs2) ->
  sub_attrs(?BGP_PATH_ATTR_ORIGIN, RouteAttrs2, RouteAttrs1).

med(RouteAttrs1, RouteAttrs2) ->
  ASPath1 = attr_value(?BGP_PATH_ATTR_AS_PATH, RouteAttrs1),
  ASPath2 = attr_value(?BGP_PATH_ATTR_AS_PATH, RouteAttrs2),
  case neighbor(ASPath1) =:= neighbor(ASPath2) of
    true  -> sub_attrs(?BGP_PATH_ATTR_MED, RouteAttrs2, RouteAttrs1);
    false -> 0
  end.

ebgp(#route_attrs{ebgp = true}, #route_attrs{ebgp = false}) ->  1;
ebgp(#route_attrs{ebgp = false}, #route_attrs{ebgp = true}) -> -1;
ebgp(#route_attrs{}, #route_attrs{}) -> 0.

bgp_id(#route_attrs{peer_bgp_id = Id1}, #route_attrs{peer_bgp_id = Id2}) ->
  tuple_cmp(Id2, Id1).

peer_addr(#route_attrs{peer_addr = Addr1}, #route_attrs{peer_addr = Addr2}) ->
  tuple_cmp(Addr2, Addr1).

% TODO raw value
neighbor(<<>>) -> 0;
neighbor(<< ?BGP_AS_PATH_SEQUENCE:8, _N:8, Asn:16, _Rest/binary >>) -> Asn.

sub_attrs(Attr, RouteAttrs1, RouteAttrs2) ->
  <<V1>> = attr_value(Attr, RouteAttrs1),
  <<V2>> = attr_value(Attr, RouteAttrs2),
  V1 - V2.

attr_value(Attr, #route_attrs{path_attrs = PathAttrs}) ->
  case dict:find(Attr, PathAttrs) of
    {ok, [Value]} -> Value#bgp_path_attr.raw_value;
    error         -> default_attr_value(Attr)
  end.

% TODO
default_attr_value(_) -> <<0>>.


tuple_cmp(T1, T2) when T1 =:= undefined orelse T2 =:= undefined ->
  0;
tuple_cmp(T1, T2) ->
  tuple_cmp({T1, tuple_size(T1)}, {T2, tuple_size(T2)}, 1).

tuple_cmp({_, 0}, {_, 0}, _) ->
  0;
tuple_cmp({_, 0}, {_, _}, _) ->
  -1;
tuple_cmp({_, _}, {_, 0}, _) ->
  1;
tuple_cmp({T1, S1}, {T2, S2}, N) ->
  case element(N, T1) - element(N, T2) of
    0 -> tuple_cmp({T1, S1-1}, {T2, S2-1}, N+1);
    X -> X
  end.
