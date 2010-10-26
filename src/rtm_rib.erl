-module(rtm_rib).
-behaviour(gen_server).

-include_lib("bgp.hrl").
-include_lib("route.hrl").

-export([start_link/0]).

% API.
-export([best_routes/0, update/3, remove_prefixes/0]).

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
update(RouteAttrs, NLRI, Withdrawn) ->
  gen_server:call(?MODULE, {update, RouteAttrs, NLRI, Withdrawn}).

-spec remove_prefixes() -> [prefix()].
remove_prefixes() ->
  gen_server:call(?MODULE, remove_prefixes).

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

handle_call({update, RouteAttrs, NLRI, Withdrawn}, {Fsm, _Tag},
            #state{rib = Rib} = State) ->
  {Added, TmpRib} = add_prefixes(Rib, RouteAttrs, NLRI),
  {Deleted, Replacements, NewRib} = del_prefixes(TmpRib, Withdrawn, Fsm),
  {reply, {Added, Deleted, Replacements}, State#state{rib = NewRib}};

handle_call(remove_prefixes, {Fsm, _Tag}, #state{rib = Rib} = State) ->
  {Removed, NewRib} = lists:foldl(fun(Prefix, {AccRemoved, AccRib}) ->
    case del_prefixes(AccRib, [Prefix], Fsm) of
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

-spec del_prefixes(rib(), [prefix()], pid()) -> {[prefix()], dict(), rib()}.
del_prefixes(Rib, Prefixes, Fsm) ->
  lists:foldl(fun(Prefix, {Deleted, Replacements, AccRib}) ->
    case remove_entry(Prefix, AccRib, Fsm) of
      {inactive, NewAccRib} ->
        {Deleted, Replacements, NewAccRib};
      {replaced, RouteAttrs, NewAccRib} ->
        {Deleted, dict:append(RouteAttrs, Prefix, Replacements), NewAccRib};
      {unfeasible, Prefix, NewAccRib} ->
        {[Prefix | Deleted], Replacements, NewAccRib}
    end
  end, {[], dict:new(), Rib}, Prefixes).

-spec remove_entry(prefix(), rib(), pid()) ->
        {replaced, #route_attrs{}, rib()} | {unfeasible, prefix(), rib()} |
        {inactive, rib()}.
remove_entry(Prefix, Rib, Fsm) ->
  case dict:find(Prefix, Rib) of
    {ok, RouteAttrsList} ->
      FromFsm = fun(#route_attrs{fsm = Owner}) -> Owner =:= Fsm end,
      {Removed, Remaining} = lists:partition(FromFsm, RouteAttrsList),
      NewRib = dict:store(Prefix, Remaining, Rib),
      case del_routes(Prefix, Removed) of
        true ->
          case add_next_best(Prefix, Remaining) of
            not_found   ->
              {unfeasible, Prefix, dict:erase(Prefix, NewRib)};
            {ok, RouteAttrs} ->
              {replaced, RouteAttrs, NewRib}
          end;
        false ->
          {inactive, NewRib}
      end;
    error ->
      error_logger:error_msg("Tried to remove nonexistant route~n"),
      {inactive, Rib}
  end.

-spec del_routes(prefix(), [#route_attrs{}]) -> boolean().
del_routes(Prefix, RouteAttrsList) ->
  del_routes(Prefix, RouteAttrsList, false).

del_routes(_Prefix, [], Removed) ->
  Removed;
del_routes(Prefix, [#route_attrs{next_hop = NextHop, active = Active} | Rest],
           Removed) ->
  case Active of
    true ->
      del_route(Prefix, NextHop),
      del_routes(Prefix, Rest, true);
    false ->
      del_routes(Prefix, Rest, Removed)
  end.

-spec add_next_best(prefix(), [#route_attrs{}]) ->
        {ok, #route_attrs{}} | not_found.
add_next_best(_Prefix, []) ->
  not_found;
add_next_best(Prefix, [#route_attrs{next_hop = NextHop,
                                    active   = false} = RouteAttrs | _]) ->
  add_route(Prefix, NextHop),
  {ok, RouteAttrs}.

-spec add_prefixes(rib(), #route_attrs{}, [prefix()]) -> {[prefix()], rib()}.
add_prefixes(Rib, #route_attrs{next_hop = NextHop} = RouteAttrs, Prefixes) ->
  InsertPrefix = fun(Prefix, {Added, AccRib}) ->
    case dict:find(Prefix, AccRib) of
      {ok, RouteAttrsList} ->
        case insert_route(Prefix, RouteAttrs, RouteAttrsList) of
          {active, NewRouteAttrsList} ->
            NewRib = dict:store(Prefix, NewRouteAttrsList, AccRib),
            {[Prefix | Added], NewRib};
          {inactive, NewRouteAttrsList} ->
            NewRib = dict:store(Prefix, NewRouteAttrsList, AccRib),
            {Added, NewRib}
        end;
      error ->
        add_route(Prefix, NextHop),
        NewRouteAttrs = RouteAttrs#route_attrs{active = true},
        NewRib = dict:store(Prefix, [NewRouteAttrs], AccRib),
        {[Prefix | Added], NewRib}
    end
  end,
  lists:foldl(InsertPrefix, {[], Rib}, Prefixes).

-spec insert_route(prefix(), #route_attrs{}, [#route_attrs{}]) ->
        {active | inactive, [#route_attrs{}]}.
insert_route(Prefix, RouteAttrs, RouteAttrsList) ->
  insert_route(Prefix, RouteAttrs, RouteAttrsList, true).

insert_route(_Prefix, NewRouteAttrs, [], false) ->
  {inactive, [NewRouteAttrs]};
insert_route(Prefix, #route_attrs{next_hop = NextHop} = NewRouteAttrs, [],
             true) ->
  add_route(Prefix, NextHop),
  {active, [NewRouteAttrs#route_attrs{active = true}]};
insert_route(Prefix, #route_attrs{next_hop = NewNextHop} = NewRouteAttrs,
             [#route_attrs{next_hop = NextHop} = RouteAttrs | Rest], First) ->
  case preference_cmp(NewRouteAttrs, RouteAttrs) of
    gt ->
      Active = case First of
        true ->
          del_route(Prefix, NextHop),
          add_route(Prefix, NewNextHop),
          true;
        false ->
          false
      end,
      ActRouteAttrs  = NewRouteAttrs#route_attrs{active = Active},
      InactRoutAttrs = RouteAttrs#route_attrs{active = false},
      {active, [ActRouteAttrs | [InactRoutAttrs | Rest]]};
    lt ->
      {inactive, NewRest} = insert_route(Prefix, NewRouteAttrs, Rest, false),
      {inactive, [RouteAttrs | NewRest]}
  end.

clear_loc_rib(Rib) ->
  lists:foreach(fun({Prefix, RouteAttrsList}) ->
    lists:foreach(fun(#route_attrs{next_hop = NextHop}) ->
      del_route(Prefix, NextHop)
    end, RouteAttrsList)
  end, dict:to_list(Rib)).

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

preference_cmp(#route_attrs{active = true} = RouteAttrs1,
               #route_attrs{active = true} = RouteAttrs2) ->
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

bgp_id(#route_attrs{bgp_id = BgpId}, #route_attrs{bgp_id = BgpId}) ->
  tuple_cmp(BgpId, BgpId).

peer_addr(#route_attrs{peer_addr = Addr1}, #route_attrs{peer_addr = Addr2}) ->
  tuple_cmp(Addr2, Addr1).

neighbor([]) -> 0;
neighbor([Asn | _Rest]) -> Asn.

sub_attrs(Attr, RouteAttrs1, RouteAttrs2) ->
  attr_value(Attr, RouteAttrs1) - attr_value(Attr, RouteAttrs2).

attr_value(Attr, #route_attrs{path_attrs = PathAttrs}) ->
  case dict:find(Attr, PathAttrs) of
    {ok, [Value]} -> Value;
    error         -> default_attr_value(Attr)
  end.

% TODO
default_attr_value(_) -> 0.


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
