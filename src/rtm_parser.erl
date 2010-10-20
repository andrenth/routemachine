-module(rtm_parser).
-include_lib("bgp.hrl").

-export([parse_header/1, parse_open/4, parse_update/3, parse_notification/1]).

%
% Parser functions.
%

-spec(parse_header(binary()) -> {ok, bgp_header()} | {error, bgp_error()}).
parse_header(?BGP_HEADER_PATTERN) ->
  Hdr = #bgp_header{
    marker   = Marker,
    msg_len  = MessageLength,
    msg_type = MessageType
  },
  case rtm_validate:header(Hdr) of
    ok    -> {ok, Hdr};
    Error -> Error
  end.

-spec(parse_open(binary(), non_neg_integer(), uint16(), ipv4_address()) ->
        {ok, bgp_open()} | {error, bgp_error()}).
parse_open(?BGP_OPEN_PATTERN, Marker, ConfigASN, ConfigID) ->
  Msg = #bgp_open{
    version        = Version,
    asn            = ASN,
    hold_time      = HoldTime,
    bgp_id         = rtm_util:num_to_ip(BGPId),
    opt_params_len = OptParamsLen,
    opt_params     = parse_opt_params(OptParams)
  },
  case rtm_validate:open(Msg, Marker, ConfigASN, ConfigID) of
    ok    -> {ok, Msg};
    Error -> Error
  end.

-spec(parse_update(binary(), uint16(), uint16()) -> {ok, bgp_update()}
                                                  | {error, bgp_error()}).
parse_update(?BGP_UPDATE_PATTERN, Len, LocalASN) ->
  Msg = #bgp_update{
    unfeasible_len   = UnfeasableLength,
    attrs_len        = TotalPathAttrLength,
    withdrawn_routes = parse_prefixes(WithdrawnRoutes),
    path_attrs       = parse_path_attrs(PathAttrs),
    nlri             = parse_prefixes(NLRI)
  },
  case rtm_validate:update(Msg, Len, LocalASN) of
    ok    -> {ok, Msg};
    Error -> Error
  end.

-spec(parse_notification(binary()) -> {ok, bgp_notification()}).
parse_notification(?BGP_NOTIFICATION_PATTERN) ->
  ErrorString = rtm_util:error_string(ErrorCode, ErrorSubCode, ErrorData),
  Msg = #bgp_notification{error_string = ErrorString},
  {ok, Msg}.

%
% Internal functions.
%

% Helpers for OPEN.

parse_opt_params(Params) ->
  parse_opt_params(Params, []).

parse_opt_params(<<>>, ParsedParams) ->
  ParsedParams;
parse_opt_params(?BGP_OPT_PARAMS_PATTERN, ParsedParams) ->
  Param = #bgp_opt_param{
    type   = ParamType,
    length = ParamLength,
    value  = ParamValue
  },
  parse_opt_params(OtherParams, [Param | ParsedParams]).


% Helpers for UPDATE.

parse_path_attrs(Attrs) ->
  parse_path_attrs(Attrs, dict:new()).

parse_path_attrs(<<>>, Parsed) ->
  Parsed;
parse_path_attrs(?BGP_PATH_ATTRS_PATTERN, Parsed) ->
  AttrLengthLength = (AttrExtended + 1) * 8,
  << AttrTypeCode : 8,
     AttrLength   : AttrLengthLength,
     AttrValue    : AttrLength/binary,
     OtherPathAttrs/binary >> = AttrRest,
  Value = parse_attr_value(AttrTypeCode, AttrValue),
  Attr = #bgp_path_attr{
    optional   = to_bool(AttrOptional),
    transitive = to_bool(AttrTransitive),
    partial    = to_bool(AttrPartial),
    extended   = to_bool(AttrExtended),
    type_code  = AttrTypeCode,
    length     = AttrLength,
    value      = Value,
    raw_value  = AttrValue % XXX this is just to ease the creation
  },                       %     of NOTIFICATION messages.
  parse_path_attrs(OtherPathAttrs, dict:append(AttrTypeCode, Attr, Parsed)).

parse_attr_value(?BGP_PATH_ATTR_ORIGIN, <<Value:8>>) ->
  Value;

parse_attr_value(?BGP_PATH_ATTR_AS_PATH, Paths) ->
  parse_as_path(Paths);

parse_attr_value(?BGP_PATH_ATTR_NEXT_HOP, <<NextHop:32>>) ->
  NextHop;

parse_attr_value(?BGP_PATH_ATTR_MED, <<MED:32>>) ->
  MED;

parse_attr_value(?BGP_PATH_ATTR_LOCAL_PREF, <<LocalPref:32>>) ->
  LocalPref;

parse_attr_value(?BGP_PATH_ATTR_ATOMIC_AGGR, <<>>) ->
  ok;

parse_attr_value(?BGP_PATH_ATTR_AGGREGATOR, <<ASN:16, BGPId:32>>) ->
  {ASN, rtm_util:num_to_ip(BGPId)}.

parse_as_path(<<>>) ->
  [];
parse_as_path(?BGP_PATH_ATTR_AS_PATH_PATTERN) ->
  [{PathType, PathASNs} | parse_as_path(OtherPaths)].

parse_prefixes(Prefixes) ->
  parse_prefixes(Prefixes, []).

parse_prefixes(<<>>, Parsed) ->
  Parsed;

parse_prefixes(?BGP_PREFIX_PATTERN, Parsed) ->
  parse_prefixes(OtherPrefixes, [{Prefix, PrefixLength} | Parsed]).

to_bool(0) -> false;
to_bool(1) -> true.
