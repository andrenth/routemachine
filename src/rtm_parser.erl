-module(rtm_parser).
-include_lib("bgp.hrl").

-export([parse_header/1, parse_open/1, parse_update/1, parse_notification/1]).

%
% Parser functions.
%

parse_header(?BGP_HEADER_PATTERN) ->
  #bgp_header{
    marker   = Marker,
    msg_len  = MessageLength,
    msg_type = MessageType
  }.

parse_open(?BGP_OPEN_PATTERN) ->
  #bgp_open{
    version        = Version,
    asn            = ASN,
    hold_time      = HoldTime,
    bgp_id         = BGPId,
    opt_params_len = OptParamsLen,
    opt_params     = parse_opt_params(OptParams)
  }.

parse_update(?BGP_UPDATE_PATTERN) ->
  {Attrs, WellKnown} = parse_path_attrs(PathAttrs),
  #bgp_update{
    unfeasible_len   = UnfeasableLength,
    attrs_len        = TotalPathAttrLength,
    withdrawn_routes = parse_withdrawn_routes(WithdrawnRoutes),
    path_attrs       = Attrs,
    well_known_attrs = WellKnown,
    nlri             = parse_nlri(NLRI)
  }.

parse_notification(?BGP_NOTIFICATION_PATTERN) ->
  Data = parse_notification(ErrorCode, ErrorSubCode, ErrorData),
  #bgp_notification{
    error_code    = ErrorCode,
    error_subcode = ErrorSubCode,
    data          = Data
  }.

%
% Internal functions.
%

% Helpers for OPEN.

parse_opt_params(Params) ->
  parse_opt_params(Params, []).

parse_opt_params(<<>>, ParsedParams) ->
  ParsedParams;

parse_opt_params(?BGP_OPT_PARAMS_PATTERN, ParsedParams) ->
  Param = parse_opt_param(ParamType, ParamValue, ParamLength),
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
  parse_path_attrs(Attrs, [], 0).

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
  parse_path_attrs(OtherPathAttrs, [Attr | Parsed], NewWellKnown).


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

parse_attr_value(?BGP_PATH_ATTR_NEXT_HOP, NextHop) ->
  NextHop;

parse_attr_value(?BGP_PATH_ATTR_MED, <<MED:32>>) ->
  MED;

parse_attr_value(?BGP_PATH_ATTR_LOCAL_PREF, <<LocalPref:32>>) ->
  LocalPref;

parse_attr_value(?BGP_PATH_ATTR_ATOMIC_AGGR, <<>>) ->
  ok;

parse_attr_value(?BGP_PATH_ATTR_AGGREGATOR, <<ASN:16, BGPId:32>>) ->
  {ASN, BGPId}.

parse_as_path(<<>>, Parsed) ->
  Parsed;

parse_as_path(?BGP_PATH_ATTR_AS_PATH_PATTERN, Parsed) ->
  parse_as_path(OtherPaths, [{PathType, PathASNs} | Parsed]).

parse_nlri(NLRI) ->
  parse_nlri(NLRI, []).

parse_nlri(<<>>, Parsed) ->
  Parsed;

parse_nlri(?BGP_NLRI_PATTERN, Parsed) ->
  parse_nlri(OtherRoutes, [ RoutePrefix | Parsed]).

%
% Helpers for NOTIFICATION.
%

% Header errors.
parse_notification(?BGP_ERR_HEADER, ?BGP_HEADER_ERR_SYNC, <<>>) ->
  undefined;
parse_notification(?BGP_ERR_HEADER, ?BGP_HEADER_ERR_LENGTH, <<BadLen>>) ->
  BadLen;
parse_notification(?BGP_ERR_HEADER, ?BGP_HEADER_ERR_TYPE, <<BadType>>) ->
  BadType;

% OPEN errors.
parse_notification(?BGP_ERR_OPEN, ?BGP_OPEN_ERR_VERSION, <<MaxVer:16>>) ->
  MaxVer;
parse_notification(?BGP_ERR_OPEN, ?BGP_OPEN_ERR_PEER_AS, <<>>) ->
  undefined;
parse_notification(?BGP_ERR_OPEN, ?BGP_OPEN_ERR_HOLD_TIME, <<>>) ->
  undefined;
parse_notification(?BGP_ERR_OPEN, ?BGP_OPEN_ERR_BGP_ID, <<>>) ->
  undefined;
parse_notification(?BGP_ERR_OPEN, ?BGP_OPEN_ERR_OPT_PARAM, <<>>) ->
  undefined;
parse_notification(?BGP_ERR_OPEN, ?BGP_OPEN_ERR_AUTH_FAIL, <<>>) ->
  undefined;

% UPDATE errors.
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_LIST, <<>>) ->
  undefined;
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_FLAGS, Data) ->
  element(1, parse_path_attrs(Data));
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_LENGTH, Data) ->
  element(1, parse_path_attrs(Data));
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_MISSING, <<Code:8>>) ->
  Code;
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_UNRECOG, Data) ->
  element(1, parse_path_attrs(Data));
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ORIGIN, Data) ->
  element(1, parse_path_attrs(Data));
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_NEXT_HOP, Data) ->
  element(1, parse_path_attrs(Data));
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_AS_PATH, <<>>) ->
  undefined;
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_OPT_ATTR, Data) ->
  element(1, parse_path_attrs(Data));
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_NETWORK, <<>>) ->
  undefined;

% Hold time errors.
parse_notification(?BGP_ERR_HOLD_TIME, _SubCode, <<>>) ->
  undefined;

% FSM errors.
parse_notification(?BGP_ERR_FSM, _SubCode, <<>>) ->
  undefined;

% Cease.
parse_notification(?BGP_ERR_CEASE, _SubCode, <<>>) ->
  undefined.
