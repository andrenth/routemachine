-module(rtm_msg).
-export([send_open/1, send_updates/5, send_updates/3,
         send_notification/2, send_keepalive/1]).
-export([build_notification/1]).

-include_lib("bgp.hrl").
-include_lib("route.hrl").
-include_lib("session.hrl").

%
% Message sending.
%

-spec send_open(#session{}) -> ok.
send_open(#session{server     = Server,
                   local_asn  = Asn,
                   hold_time  = HoldTime,
                   local_addr = LocalAddr}) ->
  % TODO allow for BgpId =/= LocalAddr.
  BgpId = rtm_util:ip_to_num(LocalAddr),
  Msg = build_open(Asn, HoldTime, BgpId),
  send(Server, Msg),
  ok.

-spec send_updates(#session{}, [pid()], dict(), [prefix()], [prefix()]) -> ok.
send_updates(Session, Servers, PathAttrs, Added, Deleted) ->
  AttrsToSend = update_path_attrs(Session, PathAttrs),
  lists:foreach(fun(Server) ->
    send_update_messages(Server, AttrsToSend, Added, Deleted)
  end, Servers),
  ok.

-spec send_updates(#session{}, [pid()], dict()) -> ok.
send_updates(Session, Servers, RouteAttrs) ->
  dict:fold(fun(#route_attrs{path_attrs = Attrs}, Prefixes, ok) ->
    AttrsToSend = update_path_attrs(Session, Attrs),
    lists:foreach(fun(Server) ->
      send_update_messages(Server, AttrsToSend, Prefixes, [])
    end, Servers)
  end, ok, RouteAttrs).

-spec send_notification(#session{}, bgp_error()) -> ok.
send_notification(#session{server = Server}, Error) ->
  send(Server, build_notification(Error)).

-spec send_keepalive(#session{}) -> ok.
send_keepalive(#session{server = Server}) ->
  send(Server, build_keepalive()).

-spec send(pid(), binary()) -> ok.
send(Server, Bin) ->
  rtm_server:send_msg(Server, Bin).

%
% Message building.
%

-spec build_header(bgp_msg_type(), uint16()) -> binary().
build_header(MessageType, MessageLength) ->
  Marker = ?BGP_HEADER_MARKER,
  ?BGP_HEADER_PATTERN.

-spec build_open(uint16(), uint16(), uint32()) -> binary().
build_open(Asn, HoldTime, BgpId) ->
  Version = 4,
  OptParamsLen = 0,   % TODO
  OptParams = <<>>,
  Len = ?BGP_OPEN_MIN_LENGTH + OptParamsLen,
  list_to_binary([build_header(?BGP_TYPE_OPEN, Len), ?BGP_OPEN_PATTERN]).

-spec build_updates(bgp_path_attrs(), [prefix()], [prefix()]) -> [binary()].
build_updates(Attrs, NewPrefixes, Withdrawn) ->
  PathAttrs = rtm_attr:attrs_to_binary(Attrs),
  TotalPathAttrLength = byte_size(PathAttrs),
  NLRI = build_prefixes(NewPrefixes),
  WithdrawnRoutes = build_prefixes(Withdrawn),
  UnfeasableLength = byte_size(WithdrawnRoutes),
  Len = ?BGP_UPDATE_MIN_LENGTH + TotalPathAttrLength + UnfeasableLength
      + byte_size(NLRI),
  case Len > ?BGP_MAX_MSG_LEN of
    true  -> split_updates(Attrs, TotalPathAttrLength, NewPrefixes, Withdrawn);
    false -> [list_to_binary([build_header(?BGP_TYPE_UPDATE, Len),
                              ?BGP_UPDATE_PATTERN])]
  end.

split_updates(Attrs, AttrsLen, New, []) ->
  Split = split_prefixes(AttrsLen, New),
  lists:foldl(fun(Ps, Acc) ->
    build_updates(Attrs, Ps, []) ++ Acc
  end, [], Split);
split_updates(Attrs, AttrsLen, [], Del) ->
  Split = split_prefixes(AttrsLen, Del),
  lists:foldl(fun(Ps, Acc) ->
    build_updates(Attrs, [], Ps) ++ Acc
  end, [], Split);
split_updates(Attrs, _AttrsLen, New, Del) ->
  build_updates(dict:new(), [], Del) ++ build_updates(Attrs, New, []).

% Split a prefix list in sublists that fit in an UPDATE message.
split_prefixes(AttrsLen, Prefixes) ->
  SplitPrefixes = fun({_Pref, Len} = Prefix, {CurLen, [Cur | Rest] = Split}) ->
    NewLen = CurLen + Len,
    case NewLen > ?BGP_MAX_MSG_LEN of
      true  -> {AttrsLen + Len, [[Prefix] | Split]};
      false -> {NewLen, [[Prefix | Cur] | Rest]}
    end
  end,
  {_, Split} = lists:foldl(SplitPrefixes, {AttrsLen, [[]]}, Prefixes),
  Split.

-spec build_notification(bgp_error()) -> binary().
build_notification({ErrorCode, ErrorSubCode, ErrorData}) ->
  Msg = ?BGP_NOTIFICATION_PATTERN,
  Len = ?BGP_HEADER_LENGTH + byte_size(Msg),
  list_to_binary([build_header(?BGP_TYPE_NOTIFICATION, Len),
                  ?BGP_NOTIFICATION_PATTERN]);
build_notification({ErrorCode, ErrorSubCode}) ->
  build_notification({ErrorCode, ErrorSubCode, <<>>});
build_notification(ErrorCode) ->
  build_notification({ErrorCode, 0, <<>>}).

-spec build_keepalive() -> binary().
build_keepalive() ->
  build_header(?BGP_TYPE_KEEPALIVE, ?BGP_HEADER_LENGTH).

build_prefixes(Prefixes) ->
  list_to_binary(lists:map(fun({Prefix, Len}) ->
    PadLen = octet_boundary_pad(Len),
    << Len:8, Prefix:Len, 0:PadLen >>
  end, Prefixes)).

%
% Internal functions.
%

% ORIGIN: always keep
% AS_PATH: update if propagating to external peer or when originating
% NEXT_HOP: update if propagating to external peer
% MED: propagate only to internal peers
% LOCAL_PREF: propagate only to internal peers
% ATOMIC_AGGREGATE: always propagate
% TODO AGGREGATOR: propagate if doing aggregation
update_path_attrs(#session{local_asn  = LocalAsn,
                           peer_asn   = PeerAsn,
                           local_addr = LocalAddr}, PathAttrs) ->
  UpdateAttrs = case LocalAsn =:= PeerAsn of
    true ->
      fun rtm_attr:update_for_ibgp/3;
    false ->
      fun(TypeCode, Attr, Attrs) ->
        Addr = rtm_util:ip_to_num(LocalAddr),
        rtm_attr:update_for_ebgp(TypeCode, Attr, Attrs, LocalAsn, Addr)
      end
  end,
  rtm_attr:fold(UpdateAttrs, PathAttrs, PathAttrs).

send_update_messages(Server, PathAttrs, Added, Deleted) ->
  Msgs = build_updates(PathAttrs, Added, Deleted),
  lists:foreach(fun(Msg) -> send(Server, Msg) end, Msgs).

octet_boundary_pad(Len) when Len > 24 -> 32 - Len;
octet_boundary_pad(Len) when Len > 16 -> 24 - Len;
octet_boundary_pad(Len) when Len > 8  -> 16 - Len;
octet_boundary_pad(Len)               ->  8 - Len.
