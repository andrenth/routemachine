-module(rtm_parser).
-include_lib("bgp.hrl").

-export([parse_header/1, parse_open/1, parse_update/1, parse_notification/1]).

-define(BGP_HEADER_MARKER, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).

-define(BGP_HEADER_PATTERN,
  << ?BGP_HEADER_MARKER : 128,
      MessageLength     : 16,
      MessageType       : 8 >>).

-define(BGP_OPEN_PATTERN,
  << Version      : 8,
     ASN          : 16,
     HoldTime     : 16,
     BGPId        : 32,
     OptParamsLen : 8,
     OptParams    : OptParamsLen/binary >>). 

-define(BGP_UPDATE_PATTERN,
  << UnfeasableLength    : 16,
     WithdrawnRoutes     : UnfeasableLength/binary,
     TotalPathAttrLength : 16,
     PathAttrs           : TotalPathAttrLength/binary,
     NLRI/binary >>).

-define(BGP_NOTIFICATION_PATTERN,
  << ErrorCode    : 8,
     ErrorSubCode : 8,
     ErrorData/binary >>).

-define(BGP_OPT_PARAMS_PATTERN,
  << ParamType   : 8,
     ParamLength : 8,
     ParamValue  : ParamLength/binary,
     OtherParams/binary >>).

-define(BGP_PARAM_AUTH_INFO_PATTERN,
  << AuthCode : 8,
     AuthData/binary >>).

-define(BGP_WITHDRAWN_ROUTES_PATTERN,
  << WithdrawnLength : 8,
     WithdrawnPrefix : WithdrawnLength/binary,
     OtherWithdrawn/binary >>).

-define(BGP_PATH_ATTRS_PATTERN,
  << AttrOptional   : 1,
     AttrTransitive : 1,
     AttrPartial    : 1,
     AttrExtended   : 1,
     0              : 4,  % unused bits
     % Can't do it all in one match because of the extended bit role in
     % defining the attribute length.
     AttrRest/binary >>).

-define(BGP_PATH_ATTR_AS_PATH_PATTERN,
  << PathType   : 8,
     PathLength : 8,
     PathASNs   : PathLength/binary-unit:16,
     OtherPaths/binary >>).

-define(BGP_NLRI_PATTERN,
  << RouteLength : 8,
     RoutePrefix : RouteLength/binary,
     OtherRoutes/binary >>).

%
% Parser functions.
%

parse_header(?BGP_HEADER_PATTERN) ->
  #bgp_header{
    message_length = MessageLength,
    message_type   = MessageType
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
  #bgp_update{
    withdrawn_routes = parse_withdrawn_routes(WithdrawnRoutes),
    path_attrs       = parse_path_attrs(PathAttrs),
    nlri             = parse_nlri(NLRI)
  }.

parse_notification(?BGP_NOTIFICATION_PATTERN) ->
  Data = parse_notification(ErrorCode, ErrorSubCode, ErrorData),
  {ErrorCode, ErrorSubCode, Data}.

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
  parse_path_attrs(Attrs, []).

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
           optional   = AttrOptional,
           transitive = AttrTransitive,
           partial    = AttrPartial,
           type_code  = AttrTypeCode,
           length     = AttrLength,
           value      = Value
         },
  parse_path_attrs(OtherPathAttrs, [Attr | Parsed]).

parse_attr_value(?BGP_PATH_ATTR_ORIGIN, <<Value:8>>) ->
  Value;

parse_attr_value(?BGP_PATH_ATTR_AS_PATH, Paths) ->
  parse_as_paths(Paths, []);

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

parse_as_paths(<<>>, Parsed) ->
  Parsed;

parse_as_paths(?BGP_PATH_ATTR_AS_PATH_PATTERN, Parsed) ->
  parse_as_paths(OtherPaths, [{PathType, PathASNs} | Parsed]).

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
  parse_path_attrs(Data);
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_LENGTH, Data) ->
  parse_path_attrs(Data);
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_MISSING, <<Code:8>>) ->
  Code;
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_UNRECOG, Data) ->
  parse_path_attrs(Data);
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ORIGIN, Data) ->
  parse_path_attrs(Data);
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_NEXT_HOP, Data) ->
  parse_path_attrs(Data);
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_AS_PATH, <<>>) ->
  undefined;
parse_notification(?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_OPT_ATTR, Data) ->
  parse_path_attrs(Data);
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
