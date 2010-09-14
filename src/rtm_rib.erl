-module(rtm_rib).
-behaviour(gen_server).

-include_lib("bgp.hrl").
-include_lib("route.hrl").

-export([start_link/1]).

% API.
-export([get/2, update/2, rdp/2, stop/1]).

% Exports for gen_server.
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2,
         code_change/3]).

-record(state, {
  peer_addr,
  adj_rib_in,
  adj_rib_out
}).

start_link(PeerAddr) ->
  gen_server:start_link(?MODULE, PeerAddr, []).

%
% API
%

get(RIB, Table) ->
  gen_server:call(RIB, {get, Table}).

update(RIB, Msg) ->
  gen_server:call(RIB, {update, Msg}).

rdp(RIB, PathAttrs) ->
  gen_server:call(RIB, {rdp, PathAttrs}).

stop(RIB) ->
  gen_server:call(RIB, stop).

%
% Callbacks for gen_server.
%

init(PeerAddr) ->
  rtm_rib_mgr:register(self()),
  {ok, #state{peer_addr   = PeerAddr,
              adj_rib_in  = dict:new(),
              adj_rib_out = dict:new()}}.

handle_call({get, adj_rib_in}, _From, #state{adj_rib_in = InRIB} = State) ->
  {reply, InRIB, State};

handle_call({update, #bgp_update{withdrawn_routes = WithdrawnRoutes,
                                 path_attrs       = PathAttrs,
                                 nlri             = NLRI}},
            _From, #state{adj_rib_in = InRIB0} = State) ->
  InRIB1 = withdraw_routes(InRIB0, WithdrawnRoutes),
  InRIB2 = add_routes(InRIB1, PathAttrs, NLRI),
  {reply, ok, State#state{adj_rib_in = InRIB2}};

% Run the routing decision process.
handle_call({rdp, PathAttrs}, _From, #state{adj_rib_in = InRIB} = State) ->
  calculate_preference_degree(PathAttrs),
  Routes = rtm_rib_mgr:select_best_routes(self(), InRIB),
  io:format("Got routes ~p~n",
    [lists:map(fun({{L, P}, #route{next_hop = NH}}) ->
      Num = P bsl (32 - L),
      IP = {(Num band 16#ff000000) bsr 24, (Num band 16#00ff0000) bsr 16,
            (Num band 16#0000ff00) bsr 8, (Num band 16#000000ff)},
      {IP, NH}
    end, Routes)]),
  ok = rtm_rib_mgr:update(Routes), 
  {reply, ok, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

terminate(_Reason, #state{peer_addr = PeerAddr}) ->
  rtm_rib_mgr:remove(PeerAddr),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_cast(_Request, State) ->
  {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
  {stop, unexpected_info, State}.

%
% Internal functions.
%

withdraw_routes(InRIB, Prefixes) ->
  lists:foldl(fun(Pref, RIB) -> dict:erase(Pref, RIB) end, InRIB, Prefixes).

add_routes(InRIB, Attrs, Prefixes) ->
  [#bgp_path_attr{value = NextHop}] =
    dict:fetch(?BGP_PATH_ATTR_NEXT_HOP, Attrs),
  InsertPrefix = fun(Pref, RIB) ->
    Route = #route{next_hop = NextHop, local_pref = local_pref(Attrs)},
    dict:store(Pref, Route, RIB)
  end,
  lists:foldl(InsertPrefix, InRIB, Prefixes).

calculate_preference_degree(PathAttrs) ->
  [#bgp_path_attr{value = Val}] = dict:fetch(?BGP_PATH_ATTR_ORIGIN, PathAttrs),
  case Val of
    ?BGP_PATH_ATTR_ORIGIN_IGP        -> local_pref(PathAttrs);
    ?BGP_PATH_ATTR_ORIGIN_EGP        -> 0;
    ?BGP_PATH_ATTR_ORIGIN_INCOMPLETE -> 0
  end.

local_pref(PathAttrs) ->
  case dict:find(?BGP_PATH_ATTR_LOCAL_PREF, PathAttrs) of
    {ok, [LocalPref]} -> LocalPref;
    error             -> 0
  end.
