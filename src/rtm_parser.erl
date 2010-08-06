-module(rtm_parser).
-include_lib("bgp.hrl").

-export([parse_header/1, parse_open/1]).

-define(BGP_HEADER_MARKER, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).

-define(BGP_HEADER_PATTERN,
  << ?BGP_HEADER_MARKER : 128,
      MessageLength     :  16,
      MessageType       :   8 >>).

-define(BGP_OPEN_PATTERN,
  << Version      : 8,
     ASN          : 16,
     HoldTime     : 16,
     BGPId        : 32,
     OptParamsLen : 8,
     OptParams    : OptParamsLen/binary >>). 

-define(BGP_OPT_PARAMS_PATTERN,
  << ParamType   : 8,
     ParamLength : 8,
     ParamValue  : ParamLength/binary,
     OtherParams/binary >>).

-define(BGP_PARAM_AUTH_INFO_PATTERN,
  << AuthCode : 8,
     AuthData/binary >>).

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

parse_opt_params(Params) ->
  parse_opt_params(Params, []).

parse_opt_params(<<>>, ParsedParams) ->
  ParsedParams;

parse_opt_params(?BGP_OPT_PARAMS_PATTERN, ParsedParams) ->
  Param = parse_opt_param(ParamType, ParamValue, ParamLength),
  parse_opt_params(OtherParams, [Param | ParsedParams]).

parse_opt_param(?BGP_PARAM_AUTH_INFO, ?BGP_PARAM_AUTH_INFO_PATTERN, _Len) ->
  #bgp_auth_info{code = AuthCode, data = AuthData}.
