-module(rtm_parser).
-include_lib("bgp.hrl").

-export([parse_header/1, parse_open/3, parse_update/3, parse_notification/1]).

%
% Parser functions.
%

parse_header(?BGP_HEADER_PATTERN) ->
  Hdr = #bgp_header{
    marker   = Marker,
    msg_len  = MessageLength,
    msg_type = MessageType
  },
  case rtm_msg:validate_header(Hdr) of
    ok    -> {ok, Hdr};
    Error -> Error
  end.

parse_open(?BGP_OPEN_PATTERN, ConfigASN, ConfigID) ->
  Msg = #bgp_open{
    version        = Version,
    asn            = ASN,
    hold_time      = HoldTime,
    bgp_id         = rtm_util:num_to_ip(BGPId),
    opt_params_len = OptParamsLen,
    opt_params     = parse_opt_params(OptParams)
  },
  case rtm_msg:validate_open(Msg, ConfigASN, ConfigID) of
    ok    -> {ok, Msg};
    Error -> Error
  end.

parse_update(?BGP_UPDATE_PATTERN, Len, LocalASN) ->
  {Attrs, WellKnown} = parse_path_attrs(PathAttrs),
  Msg = #bgp_update{
    unfeasible_len   = UnfeasableLength,
    attrs_len        = TotalPathAttrLength,
    withdrawn_routes = parse_withdrawn_routes(WithdrawnRoutes),
    path_attrs       = Attrs,
    well_known_attrs = WellKnown,
    nlri             = parse_nlri(NLRI)
  },
  case rtm_msg:validate_update(Msg, Len, LocalASN) of
    ok    -> {ok, Msg};
    Error -> Error
  end.

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
  % TODO For now don't parse individual opt params.
  %Param = parse_opt_param(ParamType, ParamValue, ParamLength),
  Param = ParamValue,
  parse_opt_params(OtherParams, [Param | ParsedParams]).

parse_opt_param(?BGP_PARAM_AUTH_INFO, ?BGP_PARAM_AUTH_INFO_PATTERN, _Len) ->
  #bgp_auth_info{code = AuthCode, data = AuthData}.

% Helpers for UPDATE.

parse_withdrawn_routes(WithdrawnRoutes) ->
  parse_withdrawn_routes(WithdrawnRoutes, []).

parse_withdrawn_routes(<<>>, Parsed) ->
  Parsed;

parse_withdrawn_routes(?BGP_WITHDRAWN_ROUTES_PATTERN, Parsed) ->
  parse_withdrawn_routes(OtherWithdrawn, [WithdrawnPrefix | Parsed]).

parse_path_attrs(Attrs) ->
  parse_path_attrs(Attrs, dict:new(), 0).

parse_path_attrs(<<>>, Parsed, WellKnown) ->
  {Parsed, WellKnown};

parse_path_attrs(?BGP_PATH_ATTRS_PATTERN, Parsed, WellKnown) ->
  AttrLengthLength = (AttrExtended + 1) * 8,
  << AttrTypeCode : 8,
     AttrLength   : AttrLengthLength,
     AttrValue    : AttrLength/binary,
     OtherPathAttrs/binary >> = AttrRest,
  Value = parse_attr_value(AttrTypeCode, AttrValue),
  Attr = #bgp_path_attr{
           optional   = AttrOptional,
           transitive = AttrTransitive,
           partial    = AttrPartial,
           extended   = AttrExtended,
           type_code  = AttrTypeCode,
           length     = AttrLength,
           value      = Value,
           raw_value  = AttrValue % XXX this is just to ease the creation
                                  %     of NOTIFICATION messages.
         },
  NewWellKnown = add_flag(WellKnown, AttrTypeCode),
  parse_path_attrs(OtherPathAttrs, dict:append(AttrTypeCode, Attr, Parsed),
                   NewWellKnown).


add_flag(Flags, ?BGP_PATH_ATTR_ORIGIN) ->
  Flags bor ?BGP_WELL_KNOWN_FLAG_ORIGIN;
add_flag(Flags, ?BGP_PATH_ATTR_AS_PATH) ->
  Flags bor ?BGP_WELL_KNOWN_FLAG_AS_PATH;
add_flag(Flags, ?BGP_PATH_ATTR_NEXT_HOP) ->
  Flags bor ?BGP_WELL_KNOWN_FLAG_NEXT_HOP;
add_flag(Flags, ?BGP_PATH_ATTR_LOCAL_PREF) ->
  Flags bor ?BGP_WELL_KNOWN_FLAG_LOCAL_PREF;
add_flag(Flags, ?BGP_PATH_ATTR_ATOMIC_AGGR) ->
  Flags bor ?BGP_WELL_KNOWN_FLAG_ATOMIC_AGGR;
add_flag(Flags, _) ->
  Flags.

parse_attr_value(?BGP_PATH_ATTR_ORIGIN, <<Value:8>>) ->
  Value;

parse_attr_value(?BGP_PATH_ATTR_AS_PATH, Paths) ->
  parse_as_path(Paths, []);

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

parse_as_path(<<>>, Parsed) ->
  Parsed;

parse_as_path(?BGP_PATH_ATTR_AS_PATH_PATTERN, Parsed) ->
  parse_as_path(OtherPaths, [{PathType, PathASNs} | Parsed]).

parse_nlri(NLRI) ->
  parse_nlri(NLRI, []).

parse_nlri(<<>>, Parsed) ->
  Parsed;

parse_nlri(?BGP_NLRI_PATTERN, Parsed) ->
  parse_nlri(OtherRoutes, [ {RouteLength, RoutePrefix} | Parsed]).
