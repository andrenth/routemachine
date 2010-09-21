-module(rtm_rib).
-behaviour(gen_server).

-include_lib("bgp.hrl").
-include_lib("route.hrl").

-export([start_link/0]).

% API.
-export([update/2, remove/1]).

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

update(Msg, Fsm) ->
  gen_server:cast(rtm_rib, {update, Msg, Fsm}).

remove(Fsm) ->
  gen_server:cast(rtm_rib, {remove, Fsm}).

%
% Callbacks for gen_server.
%

init(ok) ->
  error_logger:info_msg("Starting RIB server ~w~n", [self()]),
  process_flag(trap_exit, true),
  register(rtm_rib, self()),
  {ok, #state{rib = dict:new()}}.

handle_cast({update, #bgp_update{withdrawn_routes = WithdrawnRoutes,
                                 path_attrs       = PathAttrs,
                                 nlri             = NLRI}, Fsm},
            #state{rib = RIB} = State) ->
  RIB1 = withdraw_routes(RIB, WithdrawnRoutes, Fsm),
  RIB2 = add_routes(RIB1, PathAttrs, NLRI, Fsm),
  {noreply, State#state{rib = RIB2}};

handle_cast({remove, Fsm}, #state{rib = RIB} = State) ->
  NewRIB = lists:foldl(fun(LenPref, AccRIB) ->
    withdraw_routes(AccRIB, [LenPref], Fsm)
  end, RIB, dict:fetch_keys(RIB)),
  {noreply, State#state{rib = NewRIB}}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {stop, unexpected_call, State}.

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

withdraw_routes(RIB, Prefixes, Fsm) ->
  lists:foldl(fun(Pref, AccRIB) ->
    remove_entry(Pref, AccRIB, Fsm)
  end, RIB, Prefixes).

remove_entry({Len, Prefix}, RIB, Fsm) ->
  case dict:find({Len, Prefix}, RIB) of
    {ok, Entries} ->
      NotFromFsm = fun({_Pref, #route{fsm = Owner}}) -> Owner =/= Fsm end,
      {Remaining, Removed} = lists:partition(NotFromFsm, Entries),
      case del_routes(Prefix, Len, Removed) of
        true -> add_next_best(Prefix, Len, Remaining);
        false -> ok
      end,
      dict:store({Len, Prefix}, Remaining, RIB);
    error ->
      error_logger:error_msg("Tried to remove nonexistant route~n"),
      RIB
  end.

del_routes(Prefix, Len, Routes) ->
  del_routes(Prefix, Len, Routes, false).

del_routes(_Prefix, _Len, [], Removed) ->
  Removed;
del_routes(Prefix, Len, [{_Pref, #route{next_hop = GW, active = Act}} | Rs],
           Removed) ->
  case Act of
    true ->
      del_route(Prefix, Len, GW),
      del_routes(Prefix, Len, Rs, true);
    false ->
      del_routes(Prefix, Len, Rs, Removed)
  end.

add_next_best(_Prefix, _Len, []) ->
  ok;
add_next_best(Prefix, Len, [#route{next_hop = GW, active = false} | _Rest]) ->
  add_route(Prefix, Len, GW).

add_routes(RIB, Attrs, Prefixes, Fsm) ->
  [#bgp_path_attr{value = GW}] = dict:fetch(?BGP_PATH_ATTR_NEXT_HOP, Attrs),
  LocalPref = local_pref(Attrs),
  Route = #route{next_hop = GW, fsm = Fsm},
  InsertPrefix = fun({Len, Prefix} = LenPref, AccRIB) ->
    case dict:find(LenPref, AccRIB) of
      {ok, Entries} ->
        NewEntries = insert_route(Entries, Prefix, Len, Route, LocalPref),
        dict:stopre(LenPref, NewEntries);
      error ->
        add_route(Prefix, Len, GW),
        dict:store(LenPref, [{LocalPref, Route#route{active = true}}], AccRIB)
    end
  end,
  lists:foldl(InsertPrefix, RIB, Prefixes).

insert_route(Entries, Prefix, Len, Route, LocalPref) ->
  insert_route(Entries, Prefix, Len, Route, LocalPref, true).

insert_route([], _Prefix, _Len, NewRoute, NewLocalPref, false) ->
  [{NewRoute, NewLocalPref}];
insert_route([{LocalPref, #route{next_hop = GW} = Route} = LR | Rest],
             Prefix, Len, #route{next_hop = NewGW} = NewRoute,
             NewLocalPref, First) ->
  case NewLocalPref > LocalPref of
    true ->
      case First of
        true ->
          del_route(Prefix, Len, GW),
          add_route(Prefix, Len, NewGW);
        false ->
          ok
      end,
      [{NewLocalPref, NewRoute} |
        [{LocalPref, Route#route{active = false}} | Rest]];
    false ->
      [LR | insert_route(Rest, Prefix, Len, NewRoute, NewLocalPref, false)]
  end.

local_pref(PathAttrs) ->
  case dict:find(?BGP_PATH_ATTR_LOCAL_PREF, PathAttrs) of
    {ok, [LocalPref]} -> LocalPref;
    error             -> 0
  end.

clear_loc_rib(RIB) ->
  lists:foreach(fun({{Length, Prefix}, Routes}) ->
    lists:foreach(fun({_Pref, #route{next_hop = GW}}) ->
      del_route(Prefix, Length, GW)
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
