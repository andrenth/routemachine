-module(rtm_watcher).
-behaviour(gen_server).

-include_lib("bgp.hrl").
-include_lib("route.hrl").

-export([start_link/1]).

% Exports for gen_server.
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
         handle_cast/2]).

% Helper types.
-type update_res()     :: [prefix()] | {[prefix()], dict()}.
-type update_fun()     :: fun((#route_attrs{}, uint32(), [prefix()]) ->
                            update_res()).
-type rib_update_fun() :: fun((#route_attrs{}, [prefix()]) -> update_res()).
-type notify_fun()     :: fun((pid(), [prefix()], uint32()) -> ok).

% Keep these in sync with the C port.
-define(RTM_CMD_ADD,   0).
-define(RTM_CMD_DEL,   1).
-define(RTM_CMD_ERR, 255).

-define(RTM_CMD_ADD_FORMAT,
  << ?RTM_CMD_ADD : 8,
     Mask         : 8,
     Destination  : Mask,
     Gateway      : 32,
     Metric       : 32,
     OtherCommands/binary >>).

-define(RTM_CMD_DEL_FORMAT,
  << ?RTM_CMD_DEL : 8,
     Mask         : 8,
     Destination  : Mask,
     Gateway      : 32,
     Metric       : 32,
     OtherCommands/binary >>).

-define(RTM_CMD_ERR_FORMAT,
  << ?RTM_CMD_ERR  : 8,
     ErrMsgLen     : 8,
     ErrMsg        : ErrMsgLen/binary,
     OtherCommands/binary >>).

-record(state, {
  networks,
  port
}).

start_link(Networks) ->
  gen_server:start_link(?MODULE, Networks, []).

%
% Callbacks for gen_server.
%

init(Networks) ->
  process_flag(trap_exit, true),
  register(?MODULE, self()),
  PrivDir = code:priv_dir(routemachine),
  Port = open_port({spawn_executable, filename:join([PrivDir, "rtm_watcher"])},
                   [binary]),
  {ok, #state{networks = Networks, port = Port}}.

handle_info({Port, {data, Bin}}, #state{port = Port} = State) ->
  handle_data(Bin, State),
  {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
  port_close(Port),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {stop, unexpected_call, State}.

handle_cast(_Request, State) ->
  {stop, unexpected_cast, State}.

%
% Internal functions.
%

handle_data(<<>>, _State) ->
  ok;
handle_data(?RTM_CMD_ADD_FORMAT, #state{networks = Nets} = State) ->
  update_rib(fun add_to_rib/3, fun added_local_route/3,
             {Destination, Mask}, Gateway, Metric, Nets),
  handle_data(OtherCommands, State);
handle_data(?RTM_CMD_DEL_FORMAT, #state{networks = Nets} = State) ->
  update_rib(fun delete_from_rib/3, fun deleted_local_route/3,
             {Destination, Mask}, Gateway, Metric, Nets),
  handle_data(OtherCommands, State);
handle_data(?RTM_CMD_ERR_FORMAT, State) ->
  error_logger:error_msg("rtm_watcher: ~s~n", [ErrMsg]),
  handle_data(OtherCommands, State);
handle_data(Data, _State) ->
  error_logger:error_msg("rtm_watcher: unexpected data ~p~n", [Data]),
  error.

-spec update_rib(update_fun(), notify_fun(), prefix(), uint32(), uint32(),
                 [prefix()]) -> ok | not_found.
update_rib(UpdateFun, NotifyFun, Prefix, NextHop, Metric, Networks) ->
  case matched_networks(Prefix, Networks) of
    [] ->
      not_found;
    Matched ->
      {Updates, PathAttrs} = UpdateFun(Matched, NextHop, Metric),
      notify_updaters(NotifyFun, Updates, PathAttrs)
  end.

add_to_rib(Prefixes, NextHop, Metric) ->
  modify_rib(fun rtm_rib:add/2, Prefixes, NextHop, Metric).

delete_from_rib(Prefixes, NextHop, Metric) ->
  modify_rib(fun rtm_rib:del/2, Prefixes, NextHop, Metric).

-spec modify_rib(rib_update_fun(), [prefix()], uint32(), uint32()) ->
        {update_res(), dict()}.
modify_rib(Fun, Prefixes, NextHop, Metric) ->
  PathAttrs = dict:from_list([
    {?BGP_PATH_ATTR_ORIGIN,   [rtm_attr:origin(?BGP_ORIGIN_IGP)]},
    {?BGP_PATH_ATTR_AS_PATH,  [rtm_attr:empty_as_path()]},
    {?BGP_PATH_ATTR_NEXT_HOP, [rtm_attr:next_hop(NextHop)]},
    {?BGP_PATH_ATTR_MED,      [rtm_attr:med(Metric)]}
  ]),
  RouteAttrs = #route_attrs{
    active       = false,
    next_hop     = NextHop,
    path_attrs   = PathAttrs,
    ebgp         = false,
    as_path_loop = false
  },
  {Fun(RouteAttrs, Prefixes), PathAttrs}.

-spec added_local_route(pid(), [prefix()], dict()) -> ok.
added_local_route(Updater, Added, PathAttrs) ->
  error_logger:info_msg("Added non-BGP prefixes ~p~n", [Added]),
  rtm_updater:added_local_route(Updater, Added, PathAttrs).

-spec deleted_local_route(pid(), {[prefix()], dict()}, dict()) -> ok.
deleted_local_route(Updater, {Deleted, _Replacements} = Update, PathAttrs) ->
  error_logger:info_msg("Withdrawn non-BGP prefixes ~p~n", [Deleted]),
  rtm_updater:deleted_local_route(Updater, Update, PathAttrs).

-spec notify_updaters(notify_fun(), update_res(), dict()) -> ok.
notify_updaters(_Fun, [], _PathAttrs) ->
  ok;
notify_updaters(Fun, Update, PathAttrs) ->
  lists:foreach(fun(Upd) -> Fun(Upd, Update, PathAttrs) end, updaters()),
  ok.

-spec matched_networks(prefix(), [prefix()]) -> [prefix()].
matched_networks(Prefix, Networks) ->
  lists:foldl(fun(Network, Matched) ->
    case prefix_match(Prefix, Network) of
      {true, MatchedPrefix} -> [MatchedPrefix | Matched];
      false -> Matched
    end
  end, [], Networks).

-spec prefix_match(prefix(), prefix()) -> {true, prefix()} | false.
prefix_match({Pref1, Len1}, {Pref2, Len2}) ->
  {Pref, Len, MinLen} = case Len1 < Len2 of
    true  -> {Pref2, Len2, Len1};
    false -> {Pref1, Len1, Len2}
  end,
  Shift = 32 - MinLen,
  case Pref1 bsr Shift =:= Pref2 bsr Shift of
    true  -> {true, {Pref, Len}};
    false -> false
  end.

updaters() ->
  pg2:get_members(updaters).
