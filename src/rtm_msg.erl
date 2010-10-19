-module(rtm_msg).
-include_lib("bgp.hrl").

-export([open/3, update/3, notification/1, keepalive/0]).

-spec header(bgp_msg_type(), uint16()) -> binary().
header(MessageType, MessageLength) ->
  Marker = ?BGP_HEADER_MARKER,
  ?BGP_HEADER_PATTERN.

-spec open(uint16(), uint16(), ipv4_address()) -> binary().
open(ASN, HoldTime, LocalAddr) ->
  Version = 4,
  BGPId = rtm_util:ip_to_num(LocalAddr),
  OptParamsLen = 0, % TODO
  OptParams = <<>>,
  Len = ?BGP_OPEN_MIN_LENGTH + OptParamsLen,
  list_to_binary([header(?BGP_TYPE_OPEN, Len), ?BGP_OPEN_PATTERN]).

-spec update(bgp_path_attrs(), prefix_list(), prefix_list()) -> binary().
update(Attrs, NewPrefixes, Withdrawn) ->
  PathAttrs = rtm_attr:attrs_to_binary(Attrs),
  TotalPathAttrLength = size(PathAttrs),
  NLRI = build_prefixes(NewPrefixes),
  WithdrawnRoutes = build_prefixes(Withdrawn),
  UnfeasableLength = size(WithdrawnRoutes),
  Len = ?BGP_UPDATE_MIN_LENGTH + TotalPathAttrLength + UnfeasableLength
      + size(NLRI),
  list_to_binary([header(?BGP_TYPE_UPDATE, Len), ?BGP_UPDATE_PATTERN]).

-spec notification(bgp_error()) -> binary().
notification({ErrorCode, ErrorSubCode, ErrorData}) ->
  Msg = ?BGP_NOTIFICATION_PATTERN,
  Len = ?BGP_HEADER_LENGTH + size(Msg),
  list_to_binary([header(?BGP_TYPE_NOTIFICATION, Len),
                  ?BGP_NOTIFICATION_PATTERN]);
notification({ErrorCode, ErrorSubCode}) ->
  notification({ErrorCode, ErrorSubCode, <<>>});
notification(ErrorCode) ->
  notification({ErrorCode, 0, <<>>}).

-spec keepalive() -> binary().
keepalive() ->
  header(?BGP_TYPE_KEEPALIVE, ?BGP_HEADER_LENGTH).

build_prefixes(Prefixes) ->
  list_to_binary(lists:map(fun({Prefix, Len}) ->
    PadLen = octet_boundary_pad(Len),
    << Len:8, Prefix:Len, 0:PadLen >>
  end, Prefixes)).

octet_boundary_pad(Len) when Len > 24 -> 32 - Len;
octet_boundary_pad(Len) when Len > 16 -> 24 - Len;
octet_boundary_pad(Len) when Len > 8  -> 16 - Len;
octet_boundary_pad(Len)               ->  8 - Len.
