-module(rtm_rib_mgr).
-behaviour(gen_server).

-include_lib("bgp.hrl").
-include_lib("route.hrl").

-export([start_link/0]).

% API.
-export([register/1, select_best_routes/2]).

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

%
% Callbacks for gen_server.
%

init(ok) ->
  error_logger:info_msg("Starting RIB server ~w~n", [self()]),
  process_flag(trap_exit, true),
  register(rtm_rib_mgr, self()),
  {ok, #state{ribs = [], loc_rib = dict:new()}}.

handle_call({register, Pid}, _From, #state{ribs = RIBs} = State) ->
  {reply, ok, State#state{ribs = [Pid | RIBs]}};

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
  Routes = lists:foldl(GetBest, CallerInRIB, RIBs),
  SortByPref = fun({_, R1}, {_, R2}) -> local_pref(R2) > local_pref(R1) end,
  {reply, lists:sort(SortByPref, dict:to_list(Routes)), State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_cast(_Request, State) ->
  {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
  {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
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
