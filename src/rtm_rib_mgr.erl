-module(rtm_rib_mgr).
-behaviour(gen_server).

-include_lib("bgp.hrl").
-include_lib("route.hrl").

-export([start_link/0]).

% API.
-export([register/1, select_best_routes/2, update/1, remove/1]).

% Exports for gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {
  ribs,
  loc_rib
}).

start_link() ->
  gen_server:start_link(?MODULE, ok, []).

%
% API.
%

register(Pid) ->
  gen_server:call(rtm_rib_mgr, {register, Pid}).

select_best_routes(Pid, InRIB) ->
  gen_server:call(rtm_rib_mgr, {select, Pid, InRIB}).

update(Routes) ->
  gen_server:call(rtm_rib_mgr, {update, Routes}).

remove(PeerAddr) ->
  gen_server:call(rtm_rib_mgr, {remove, PeerAddr}).

%
% Callbacks for gen_server.
%

init(ok) ->
  error_logger:info_msg("Starting RIB server ~w~n", [self()]),
  process_flag(trap_exit, true),
  register(rtm_rib_mgr, self()),
  {ok, #state{ribs = sets:new(), loc_rib = dict:new()}}.

handle_call({register, Pid}, _From, #state{ribs = RIBs} = State) ->
  {reply, ok, State#state{ribs = sets:add_element(Pid, RIBs)}};

handle_call({select, CallerPid, CallerInRIB}, _From,
            #state{ribs = RIBs} = State) ->
  GetBest = fun(Pid, BestRoutes) ->
    case Pid =:= CallerPid of
      true  -> BestRoutes;
      false ->
        InRIB = rtm_rib:get(Pid, adj_rib_in),
        AddPrefix = fun(Prefix, Route, Best) ->
            case dict:find(Prefix, Best) of
              {ok, CurBest} -> add_best_route(Best, Prefix, Route, CurBest);
              _             -> dict:store(Prefix, Route, Best)
            end
        end,
        dict:fold(AddPrefix, BestRoutes, InRIB)
    end
  end,
  Routes = sets:fold(GetBest, CallerInRIB, RIBs),
  SortByPref = fun({_, R1}, {_, R2}) -> local_pref(R2) > local_pref(R1) end,
  {reply, lists:sort(SortByPref, dict:to_list(Routes)), State};

handle_call({update, Routes}, _From, #state{loc_rib = LocRIB} = State) ->
  NewLocRIB = send_updates(LocRIB, Routes),
  {reply, ok, State#state{loc_rib = NewLocRIB}};

handle_call({remove, Addr}, {Pid, _Tag}, #state{ribs    = RIBs,
                                                loc_rib = LocRIB} = State) ->
  RmGW = rtm_util:ip_to_num(Addr),
  RemoveRoutes = fun({Length, Prefix}, #route{next_hop = GW}) ->
    case GW =:= RmGW of
      true ->
        delete_route(Prefix, Length, GW),
        false;
      false ->
        true
    end
  end,
  NewLocRIB = dict:filter(RemoveRoutes, LocRIB),
  NewRIBs = sets:del_element(Pid, RIBs),
  {reply, ok, State#state{ribs = NewRIBs, loc_rib = NewLocRIB}}.

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

terminate(_Reason, #state{loc_rib = LocRIB}) ->
  clear_loc_rib(LocRIB),
  ok.

%
% Internal functions.
%

add_best_route(Routes, Prefix, R1, R2) ->
  case local_pref(R1) > local_pref(R2) of
    true  -> dict:store(Routes, Prefix, R1);
    false -> dict:store(Routes, Prefix, R2)
  end.

local_pref(#route{local_pref = LocalPref}) ->
  LocalPref.

send_updates(LocRIB, []) ->
  LocRIB;
send_updates(LocRIB, [{{Len, Prefix}, #route{next_hop = GW} = Route} | Rest]) ->
  add_route(Prefix, Len, GW),
  send_updates(dict:store({Len, Prefix}, Route, LocRIB), Rest).

clear_loc_rib(LocRIB) ->
  lists:foreach(fun({{Length, Prefix}, #route{next_hop = GW}}) ->
    delete_route(Prefix, Length, GW)
  end, dict:to_list(LocRIB)).

add_route(Pref, Len, GW) ->
  modify_route("add", Pref, Len, GW).

delete_route(Pref, Len, GW) ->
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
