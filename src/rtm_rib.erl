-module(rtm_rib).
-behaviour(gen_server).

-include_lib("bgp.hrl").
-include_lib("route.hrl").

-export([start_link/0]).

% API.
-export([get/0, update/3, remove_prefixes/0]).

% Exports for gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {
  rib
}).

start_link() ->
  gen_server:start_link(?MODULE, ok, []).

%
% API.
%

get() ->
  gen_server:call(?MODULE, get).

update(Route, NLRI, Withdrawn) ->
  gen_server:call(?MODULE, {update, Route, NLRI, Withdrawn}).

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

handle_call(get, _From, #state{rib = RIB} = State) ->
  Routes = dict:fold(fun({Prefix, Len}, Entries, Best) ->
    case Entries of
      [Route | _Rest] -> dict:append(Route, {Prefix, Len}, Best);
      [] -> Best
    end
  end, dict:new(), RIB),
  {reply, Routes, State};

handle_call({update, Route, NLRI, Withdrawn}, {FSM, _Tag},
            #state{rib = RIB} = State) ->
  {Added, TmpRIB} = add_prefixes(RIB, Route, NLRI),
  {Deleted, Replacements, NewRIB} = del_prefixes(TmpRIB, Withdrawn, FSM),
  {reply, {Added, Deleted, Replacements}, State#state{rib = NewRIB}};

handle_call(remove_prefixes, {FSM, _Tag}, #state{rib = RIB} = State) ->
  {Removed, NewRIB} = lists:foldl(fun({Pref, Len}, {AccRemoved, AccRIB}) ->
    case del_prefixes(AccRIB, [{Pref, Len}], FSM) of
      {[Deleted], _, NewAccRIB} -> {[Deleted | AccRemoved], NewAccRIB};
      {[], _, NewAccRIB}        -> {AccRemoved, NewAccRIB}
    end
  end, {[], RIB}, dict:fetch_keys(RIB)),
  {reply, Removed, State#state{rib = NewRIB}}.

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

terminate(_Reason, #state{rib = RIB}) ->
  clear_loc_rib(RIB),
  ok.

%
% Internal functions.
%

del_prefixes(RIB, Prefixes, FSM) ->
  lists:foldl(fun({Pref, Len}, {Deleted, Replacements, AccRIB}) ->
    case remove_entry({Pref, Len}, AccRIB, FSM) of
      {inactive, NewAccRIB} ->
        {Deleted, Replacements, NewAccRIB};
      {replaced, Route, NewAccRIB} ->
        {Deleted, dict:append(Route, {Pref, Len}, Replacements), NewAccRIB};
      {unfeasible, {Pref, Len}, NewAccRIB} ->
        {[{Pref, Len} | Deleted], Replacements, NewAccRIB}
    end
  end, {[], dict:new(), RIB}, Prefixes).

remove_entry({Prefix, Len}, RIB, FSM) ->
  case dict:find({Prefix, Len}, RIB) of
    {ok, Entries} ->
      FromFSM = fun(#route{fsm = Owner}) -> Owner =:= FSM end,
      {Removed, Remaining} = lists:partition(FromFSM, Entries),
      NewRIB = dict:store({Prefix, Len}, Remaining, RIB),
      case del_routes(Prefix, Len, Removed) of
        true ->
          case add_next_best(Prefix, Len, Remaining) of
            not_found   ->
              {unfeasible, {Prefix, Len}, dict:erase({Prefix, Len}, NewRIB)};
            {ok, Route} ->
              {replaced, Route, NewRIB}
          end;
        false ->
          {inactive, NewRIB}
      end;
    error ->
      error_logger:error_msg("Tried to remove nonexistant route~n"),
      {inactive, RIB}
  end.

del_routes(Prefix, Len, Routes) ->
  del_routes(Prefix, Len, Routes, false).

del_routes(_Prefix, _Len, [], Removed) ->
  Removed;
del_routes(Prefix, Len, [#route{next_hop = NextHop, active = Active} | Rs],
           Removed) ->
  case Active of
    true ->
      del_route(Prefix, Len, NextHop),
      del_routes(Prefix, Len, Rs, true);
    false ->
      del_routes(Prefix, Len, Rs, Removed)
  end.

add_next_best(_Prefix, _Len, []) ->
  not_found;
add_next_best(Prefix, Len,
              [#route{next_hop = NextHop, active = false} = Route | _]) ->
  add_route(Prefix, Len, NextHop),
  {ok, Route}.

add_prefixes(RIB, #route{next_hop = NextHop} = Route, Prefixes) ->
  InsertPrefix = fun({Pref, Len}, {Added, AccRIB}) ->
    case dict:find({Pref, Len}, AccRIB) of
      {ok, Entries} ->
        case insert_route(Entries, Pref, Len, Route) of
          {active, NewEntries} ->
            NewRIB = dict:store({Pref, Len}, NewEntries, AccRIB),
            {[{Pref, Len} | Added], NewRIB};
          {inactive, NewEntries} ->
            NewRIB = dict:store({Pref, Len}, NewEntries, AccRIB),
            {Added, NewRIB}
        end;
      error ->
        add_route(Pref, Len, NextHop),
        NewRIB = dict:store({Pref, Len}, [Route#route{active = true}], AccRIB),
        {[{Pref, Len} | Added], NewRIB}
    end
  end,
  lists:foldl(InsertPrefix, {[], RIB}, Prefixes).

insert_route(Entries, Prefix, Len, Route) ->
  insert_route(Entries, Prefix, Len, Route, true).

insert_route([], _Prefix, _Len, NewRoute, false) ->
  {inactive, [NewRoute]};
insert_route([], Prefix, Len, #route{next_hop = NextHop} = NewRoute, true) ->
  add_route(Prefix, Len, NextHop),
  {active, [NewRoute#route{active = true}]};
insert_route([#route{next_hop = NextHop} = Route | OtherRoutes],
             Prefix, Len, #route{next_hop = NewNextHop} = NewRoute, First) ->
  case preference_cmp(NewRoute, Route) of
    gt ->
      Active = case First of
        true ->
          del_route(Prefix, Len, NextHop),
          add_route(Prefix, Len, NewNextHop),
          true;
        false ->
          false
      end,
      ActiveRoute = NewRoute#route{active = Active},
      InactiveRoute = Route#route{active = false},
      {active, [ActiveRoute | [InactiveRoute | OtherRoutes]]};
    lt ->
      {inactive, NewOtherRoutes} = insert_route(OtherRoutes, Prefix, Len,
                                                NewRoute, false),
      {inactive, [Route | NewOtherRoutes]}
  end.

clear_loc_rib(RIB) ->
  lists:foreach(fun({{Prefix, Length}, Routes}) ->
    lists:foreach(fun(#route{next_hop = NextHop}) ->
      del_route(Prefix, Length, NextHop)
    end, Routes)
  end, dict:to_list(RIB)).

add_route(Pref, Len, GW) ->
  io:format("Adding route ~p/~p via ~p~n", [Pref, Len, GW]),
  modify_route("add", Pref, Len, GW).

del_route(Pref, Len, GW) ->
  io:format("Deleting route ~p/~p via ~p~n", [Pref, Len, GW]),
  modify_route("del", Pref, Len, GW).

modify_route(Cmd, Pref, Len, GW) ->
  Dst = Pref bsl (32 - Len),
  run_rtm(Cmd, Len, Dst, GW).

run_rtm(Cmd, Len, Dst, GW) ->
  Args = [Cmd, integer_to_list(Len), integer_to_list(Dst), integer_to_list(GW)],
  PrivDir = code:priv_dir(routemachine),
  Port = open_port({spawn_executable, filename:join([PrivDir, "rtm"])},
                   [stream, {args, Args}]),
  erlang:port_close(Port).

preference_cmp(#route{active = true} = Route1,
               #route{active = true} = Route2) ->
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
  compare_in_order(Attributes, Route1, Route2).

compare_in_order([Fun | Rest], Route1, Route2) ->
  case Fun(Route1, Route2) of
    X when X > 0 -> gt;
    X when X < 0 -> lt;
    0 -> compare_in_order(Rest, Route1, Route2)
  end.

local_pref(Route1, Route2) ->
  sub_attrs(?BGP_PATH_ATTR_LOCAL_PREF, Route1, Route2).

as_path_len(Route1, Route2) ->
  ASPath1 = attr_value(?BGP_PATH_ATTR_AS_PATH, Route1),
  ASPath2 = attr_value(?BGP_PATH_ATTR_AS_PATH, Route2),
  size(ASPath2) - size(ASPath1).

origin(Route1, Route2) ->
  sub_attrs(?BGP_PATH_ATTR_ORIGIN, Route2, Route1).

med(Route1, Route2) ->
  ASPath1 = attr_value(?BGP_PATH_ATTR_AS_PATH, Route1),
  ASPath2 = attr_value(?BGP_PATH_ATTR_AS_PATH, Route2),
  case neighbor(ASPath1) =:= neighbor(ASPath2) of
    true  -> sub_attrs(?BGP_PATH_ATTR_MED, Route2, Route1);
    false -> 0
  end.

ebgp(#route{ebgp = true}, #route{ebgp = false}) ->  1;
ebgp(#route{ebgp = false}, #route{ebgp = true}) -> -1;
ebgp(#route{}, #route{}) -> 0.

bgp_id(#route{bgp_id = BGPId1}, #route{bgp_id = BGPId2}) ->
  BGPId2 - BGPId1.

peer_addr(#route{peer_addr = PeerAddr1}, #route{peer_addr = PeerAddr2}) ->
  PeerAddr2 - PeerAddr1.

neighbor([]) -> 0;
neighbor([ASN | _Rest]) -> ASN.

sub_attrs(Attr, Route1, Route2) ->
  attr_value(Attr, Route1) - attr_value(Attr, Route2).

attr_value(Attr, #route{path_attrs = PathAttrs}) ->
  case dict:find(Attr, PathAttrs) of
    {ok, [Value]} -> Value;
    error         -> default_attr_value(Attr)
  end.

% TODO
default_attr_value(_) -> 0.
