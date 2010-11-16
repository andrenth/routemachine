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
        {[prefix()], [prefix()], [{prefix(), #route_attrs{}}], rib()}.
update(RouteAttrs, Nlri, Withdrawn) ->
  gen_server:call(?MODULE, {update, RouteAttrs, Nlri, Withdrawn}).

-spec remove_prefixes(uint32()) -> {[prefix()], [dict()]}.
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
  {Added, [], NewRib} = add_prefixes(Rib, RouteAttrs, Prefixes),
  {reply, Added, State#state{rib = NewRib}};

handle_call({del, #route_attrs{peer_bgp_id = PeerBgpId}, Prefixes}, _From,
            #state{rib = Rib} = State) ->
  {Deleted, Replacements, NewRib} = del_prefixes(Rib, Prefixes, PeerBgpId),
  {reply, {Deleted, Replacements}, State#state{rib = NewRib}};

handle_call({update, #route_attrs{peer_bgp_id = PeerBgpId,
                                  next_hop    = NextHop} = RouteAttrs,
             Nlri, Withdrawn}, _From, #state{rib = Rib} = State) ->
  {Added, Replaced, TmpRib} = add_prefixes(Rib, RouteAttrs, Nlri),
  {Deleted, Replacements, NewRib} = del_prefixes(TmpRib, Withdrawn, PeerBgpId),
  lists:foreach(fun({Prefix, #route_attrs{next_hop = Gw}}) ->
    del_route(Prefix, Gw)
  end, Replaced),
  add_routes(Added, NextHop),
  del_routes(Deleted, NextHop),
  add_replacements(Replacements),
  {reply, {Added, Replaced, Deleted, Replacements}, State#state{rib = NewRib}};

handle_call({remove_prefixes, PeerId}, _From, #state{rib = Rib} = State) ->
  {Deleted, Replacements, NewRib} =
    lists:foldl(fun({Prefix, Rtas}, {AccDel, AccRepl, AccRib}) ->
      case remove_peer_routes(Rtas, PeerId, Prefix, AccRib) of
        {Del, Repl, NewAccRib} -> {[Del | AccDel], [Repl | AccRepl], NewAccRib};
        none -> {AccDel, AccRepl, AccRib}
      end
    end, {[], [], Rib}, dict:to_list(Rib)),
  {reply, {Deleted, Replacements}, State#state{rib = NewRib}}.

-spec remove_peer_routes([#route_attrs{}], uint32(), prefix(), rib()) ->
        {[prefix()], dict(), rib()} | none.
remove_peer_routes([], _PeerBgpId, _Prefix, _Rib) ->
  none;
remove_peer_routes([Rta | Rest], PeerBgpId, Prefix, Rib) ->
  #route_attrs{next_hop = NextHop, peer_bgp_id = Id} = Rta,
  case Id =:= PeerBgpId of
    true ->
      {Del, Repl, NewRib} = del_prefixes(Rib, [Prefix], Id),
      del_routes(Del, NextHop),
      add_replacements(Repl),
      {Del, Repl, NewRib};
    false ->
      remove_peer_routes(Rest, PeerBgpId, Prefix, Rib)
  end.

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
        NewReplacements = dict:append(RouteAttrs, Prefix, Replacements),
        {[Prefix | Deleted], NewReplacements, NewAccRib};
      {unfeasible, NewAccRib} ->
        {[Prefix | Deleted], Replacements, NewAccRib}
    end
  end, {[], dict:new(), Rib}, Prefixes).

-spec remove_entry(prefix(), rib(), ipv4_address()) ->
        {replaced, #route_attrs{}, rib()} | {unfeasible, rib()} |
        {inactive, rib()}.
remove_entry(Prefix, Rib, PeerBgpId) ->
  case dict:find(Prefix, Rib) of
    {ok, RouteAttrsList} ->
      FromId = fun(#route_attrs{peer_bgp_id = Id}) -> Id =:= PeerBgpId end,
      {Removed, Remaining} = lists:partition(FromId, RouteAttrsList),
      case removed_active(Removed) of
        true ->
          case Remaining of
            [] ->
              {unfeasible, dict:erase(Prefix, Rib)};
            [#route_attrs{active = false} = RouteAttrs | Rest] ->
              Active = RouteAttrs#route_attrs{active = true},
              NewRib = dict:store(Prefix, [Active | Rest], Rib),
              {replaced, RouteAttrs, NewRib}
          end;
        false ->
          NewRib = dict:store(Prefix, Remaining, Rib),
          {inactive, NewRib}
      end;
    error ->
      error_logger:error_msg("Tried to remove nonexistant route~n"),
      {inactive, Rib}
  end.

-spec removed_active([#route_attrs{}]) -> boolean().
removed_active([#route_attrs{active = true}]) -> true;
removed_active(_)                             -> false.

-spec add_prefixes(rib(), #route_attrs{}, [prefix()]) ->
        {[prefix()], [prefix()], rib()}.
add_prefixes(Rib, RouteAttrs, Prefixes) ->
  InsertPrefix = fun(Prefix, {Added, Deleted, AccRib}) ->
    case dict:find(Prefix, AccRib) of
      {ok, RouteAttrsList} ->
        case insert_route(RouteAttrs, RouteAttrsList) of
          {new, NewRouteAttrsList} ->
            NewRib = dict:store(Prefix, NewRouteAttrsList, AccRib),
            {[Prefix | Added], Deleted, NewRib};
          {inactive, NewRouteAttrsList} ->
            NewRib = dict:store(Prefix, NewRouteAttrsList, AccRib),
            {Added, Deleted, NewRib};
          {implicit_replacement, NewRouteAttrsList} ->
            NewRib = dict:store(Prefix, NewRouteAttrsList, AccRib),
            {[Prefix | Added], Deleted, NewRib};
          {explicit_replacement, [_, OldBest | _] = NewRouteAttrsList} ->
            NewRib = dict:store(Prefix, NewRouteAttrsList, AccRib),
            {[Prefix | Added], [{Prefix, OldBest} | Deleted], NewRib};
          equal ->
            {Added, Deleted, Rib}
        end;
      error ->
        NewRouteAttrs = RouteAttrs#route_attrs{active = true},
        NewRib = dict:store(Prefix, [NewRouteAttrs], AccRib),
        {[Prefix | Added], Deleted, NewRib}
    end
  end,
  lists:foldl(InsertPrefix, {[], [], Rib}, Prefixes).

-spec insert_route(#route_attrs{}, [#route_attrs{}]) ->
          equal
        | {new, [#route_attrs{}]}
        | {inactive, [#route_attrs{}]}
        | {implicit_replacement, [#route_attrs{}]}
        | {explicit_replacement, [#route_attrs{}]}.
insert_route(Rta, []) ->
  {new, [Rta#route_attrs{active = true}]};
insert_route(Rta, [OldBest | _Rest] = Rtas) ->
  case remove_replaced_route(Rta, Rtas) of
    kept ->
      equal;
    TmpRtas ->
      case insert(Rta, TmpRtas, true) of
        equal                   -> equal;
        [OldBest | _] = NewRtas -> {inactive, NewRtas};
        [NewBest | _] = NewRtas -> return_replacement(NewBest, OldBest, NewRtas)
      end
  end.

-spec remove_replaced_route(#route_attrs{}, [#route_attrs{}]) ->
        kept | [#route_attrs{}].
remove_replaced_route(_NewRta, []) ->
  [];
remove_replaced_route(#route_attrs{peer_bgp_id = Id,
                                   path_attrs  = NewAttrs},
                      [#route_attrs{peer_bgp_id = Id,
                                    path_attrs  = OldAttrs} | Rest]) ->
  case NewAttrs =:= OldAttrs of
    true  -> kept;
    false -> Rest
  end;
remove_replaced_route(NewRta, [Rta | Rest]) ->
  [Rta | remove_replaced_route(NewRta, Rest)].


-spec insert(#route_attrs{}, [#route_attrs{}], boolean()) ->
        equal | [#route_attrs{}].
insert(NewRta, [], Active) ->
  [NewRta#route_attrs{active = Active}];
insert(NewRta, [Rta | Rest], Active) ->
  case preference_cmp(NewRta, Rta) of
    gt ->
      Better = NewRta#route_attrs{active = Active},
      Worse = Rta#route_attrs{active = false},
      [Better, Worse | Rest];
    lt ->
      [Rta | insert(NewRta, Rest, false)];
    eq ->
      equal
  end.

return_replacement(#route_attrs{peer_bgp_id = Id},
                   #route_attrs{peer_bgp_id = Id}, Rtas) ->
  {implicit_replacement, Rtas};
return_replacement(_NewBest, _OldBest, Rtas) ->
  {explicit_replacement, Rtas}.

clear_loc_rib(Rib) ->
  lists:foreach(fun({Prefix, [#route_attrs{next_hop = NextHop,
                                           peer_bgp_id = PeerId} | _Rest]}) ->
    case PeerId =:= undefined of
      true  -> ok;  % don't delete non-BGP routes.
      false -> del_route(Prefix, NextHop)
    end
  end, dict:to_list(Rib)).

add_routes(Prefixes, NextHop) ->
  lists:foreach(fun(Prefix) -> add_route(Prefix, NextHop) end, Prefixes).

del_routes(Prefixes, NextHop) ->
  lists:foreach(fun(Prefix) -> del_route(Prefix, NextHop) end, Prefixes).

add_replacements(Replacements) ->
  dict:fold(fun(#route_attrs{next_hop = NextHop}, Prefixes, ok) ->
    add_routes(Prefixes, NextHop)
  end, ok, Replacements).

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

-spec preference_cmp(#route_attrs{active :: false},
                     #route_attrs{active ::  true}) -> lt | eq | gt.
preference_cmp(#route_attrs{active = false} = RouteAttrs1,
               #route_attrs{active = true}  = RouteAttrs2) ->
  % TODO check reachability
  Attributes = [
    fun as_path_loop/2,
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

-spec compare_in_order([fun((#route_attrs{}, #route_attrs{}) -> integer())],
                       #route_attrs{}, #route_attrs{}) -> lt | eq | gt.
compare_in_order([], _RouteAttrs1, _RouteAttrs2) ->
  eq;
compare_in_order([Fun | Rest], RouteAttrs1, RouteAttrs2) ->
  case Fun(RouteAttrs1, RouteAttrs2) of
    X when X > 0 -> gt;
    X when X < 0 -> lt;
    0 -> compare_in_order(Rest, RouteAttrs1, RouteAttrs2)
  end.

as_path_loop(#route_attrs{as_path_loop = L1},
             #route_attrs{as_path_loop = L2}) ->
  case {L1, L2} of
    {false, false} ->  0;
    {true,      _} -> -1;
    {_,      true} ->  1
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
ebgp(#route_attrs{}, #route_attrs{})                        ->  0.

bgp_id(#route_attrs{peer_bgp_id = Id1}, #route_attrs{peer_bgp_id = Id2}) ->
  Id2 - Id1.

peer_addr(#route_attrs{peer_addr = Addr1}, #route_attrs{peer_addr = Addr2}) ->
  Addr2 - Addr1.

% TODO raw value
neighbor(<<>>) -> 0;
neighbor(<< ?BGP_AS_PATH_SEQUENCE:8, _N:8, Asn:16, _Rest/binary >>) -> Asn.

sub_attrs(Attr, RouteAttrs1, RouteAttrs2) ->
  <<V1>> = attr_value(Attr, RouteAttrs1),
  <<V2>> = attr_value(Attr, RouteAttrs2),
  V1 - V2.

attr_value(Attr, #route_attrs{path_attrs = PathAttrs}) ->
  case dict:find(Attr, PathAttrs) of
    {ok, [Value]} -> Value#bgp_path_attr.binary;
    error         -> default_attr_value(Attr)
  end.

% TODO
default_attr_value(_) -> <<0>>.
