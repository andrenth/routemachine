-module(rtm_msg).
-include_lib("bgp.hrl").

-export([validate_header/1, validate_open/4, validate_update/3]).
-export([build_open/3, build_update/3, build_notification/1,
         build_keepalive/0]).

%
% Validations.
%

validate_header(Hdr) ->
  validate(Hdr, ?BGP_ERR_HEADER,
           [fun validate_marker/1,
           fun validate_msg_len/1,
           fun validate_type/1]).

validate_open(Msg, Marker, ConfigASN, ConfigID) ->
  validate(Msg, ?BGP_ERR_OPEN,
           [fun validate_version/1,
            fun(M) -> validate_asn(M, ConfigASN) end,
            fun validate_hold_time/1,
            fun(M) -> validate_bgp_id(M, ConfigID) end,
            fun(M) -> validate_opt_params(M, Marker) end]).

validate_update(Msg, MsgLen, LocalASN) ->
  validate(Msg, ?BGP_ERR_UPDATE,
           [fun(M) -> validate_update_length(M, MsgLen) end,
            fun(M) -> validate_path_attrs(M, LocalASN) end,
            fun validate_missing_well_known_attrs/1]).

%
% Internal functions.
%

% Header validation.

validate_marker(#bgp_header{msg_type = ?BGP_TYPE_OPEN,
                            marker = ?BGP_HEADER_MARKER}) ->
  ok;
validate_marker(#bgp_header{msg_type = ?BGP_TYPE_OPEN}) ->
  {error, ?BGP_HEADER_ERR_SYNC};
validate_marker(#bgp_header{}) ->
  ok.

validate_msg_len(#bgp_header{msg_type = ?BGP_TYPE_OPEN, msg_len = Len})
                 when Len >= ?BGP_OPEN_MIN_LENGTH andalso
                      Len =< ?BGP_MAX_MSG_LEN ->
  ok;
validate_msg_len(#bgp_header{msg_type = ?BGP_TYPE_UPDATE, msg_len = Len})
                 when Len >= ?BGP_UPDATE_MIN_LENGTH andalso
                      Len =< ?BGP_MAX_MSG_LEN ->
  ok;
validate_msg_len(#bgp_header{msg_type = ?BGP_TYPE_NOTIFICATION, msg_len = Len})
                 when Len >= ?BGP_NOTIFICATION_MIN_LENGTH andalso
                      Len =< ?BGP_MAX_MSG_LEN ->
  ok;
validate_msg_len(#bgp_header{msg_type = ?BGP_TYPE_KEEPALIVE, msg_len = Len})
                 when Len =:= ?BGP_HEADER_LENGTH ->
  ok;
validate_msg_len(#bgp_header{msg_len = Len}) ->
  {error, {?BGP_HEADER_ERR_LENGTH, <<Len:8>>}}.

validate_type(#bgp_header{msg_type = ?BGP_TYPE_OPEN})         -> ok;
validate_type(#bgp_header{msg_type = ?BGP_TYPE_UPDATE})       -> ok;
validate_type(#bgp_header{msg_type = ?BGP_TYPE_NOTIFICATION}) -> ok;
validate_type(#bgp_header{msg_type = ?BGP_TYPE_KEEPALIVE})    -> ok;
validate_type(#bgp_header{msg_type = Type}) ->
  {error, {?BGP_HEADER_ERR_TYPE, <<Type:8>>}}.

% OPEN message validation.

validate_version(#bgp_open{version = 4}) ->
  ok;
validate_version(#bgp_open{}) ->
  {error, {?BGP_OPEN_ERR_VERSION, <<4:16>>}}.

validate_asn(#bgp_open{asn = ASN}, ConfigASN) ->
  case ASN =:= ConfigASN of
    true  -> ok;
    false -> {error, ?BGP_OPEN_ERR_PEER_AS}
  end.

validate_hold_time(#bgp_open{hold_time = HoldTime}) when HoldTime < 3 ->
  {error, ?BGP_OPEN_ERR_PEER_AS};
validate_hold_time(#bgp_open{}) ->
  ok.

validate_bgp_id(#bgp_open{bgp_id = Id}, ConfigID) ->
  case Id =:= ConfigID of
    true  -> ok;
    false -> {error, ?BGP_OPEN_ERR_BGP_ID}
  end.

validate_opt_params(#bgp_open{opt_params = OptParams}, Marker) ->
  validate_params(OptParams, Marker).

validate_params([], _Marker) ->
  ok;
validate_params([#bgp_opt_param{type = Type, length = Len, value = Val} | Rest],
                Marker) ->
  validate_param(Type, Val, Len, Marker),
  validate_params(Rest, Marker).

validate_param(?BGP_PARAM_AUTH_INFO, ?BGP_PARAM_AUTH_INFO_PATTERN,
               Len, Marker) ->
  % TODO validate authentication info.
  {_,_,_,_} = {AuthCode, AuthData, Len, Marker},
  ok;
validate_param(_, _, _, _) ->
  {error, ?BGP_OPEN_ERR_OPT_PARAM}.


% UPDATE message validation.
% TODO optional attributes; NLRI.

validate_update_length(#bgp_update{unfeasible_len = ULen, attrs_len = ALen},
                       MsgLen) ->
  case ?BGP_UPDATE_MIN_LENGTH + ULen + ALen > MsgLen + ?BGP_HEADER_LENGTH of
    true  -> {error, ?BGP_UPDATE_ERR_ATTR_LIST};
    false -> ok
  end.

validate_path_attrs(#bgp_update{path_attrs = PathAttrs}, LocalASN) ->
  try
    dict:fold(fun(_Type, Attr, ok) ->
      validate_attr(Attr, LocalASN)
    end, ok, PathAttrs)
  catch
    throw:Error -> Error
  end.

validate_attr([Attr], LocalASN) ->
  Vs = [fun validate_flags/1,
        fun validate_attr_len/1,
        fun(A) -> validate_value(A, LocalASN) end],
  case validate(Attr, ?BGP_ERR_UPDATE, Vs) of
    ok -> ok;
    {error, {_, SubCode, Data}} -> throw({error, {SubCode, Data}})
  end;

validate_attr([_Attr | _More], _LocalASN) ->
  % Duplicate attribute.
  throw({error, ?BGP_UPDATE_ERR_ATTR_LIST}).


validate_flags(Attr) ->
  case Attr of
    #bgp_path_attr{type_code  = ?BGP_PATH_ATTR_ORIGIN,
                   optional   = 0,
                   transitive = 1} ->
      ok;
    #bgp_path_attr{type_code  = ?BGP_PATH_ATTR_AS_PATH,
                   optional   = 0,
                   transitive = 1} ->
      ok;
    #bgp_path_attr{type_code  = ?BGP_PATH_ATTR_NEXT_HOP,
                   optional   = 0,
                   transitive = 1} ->
      ok;
    #bgp_path_attr{type_code  = ?BGP_PATH_ATTR_MED,
                   optional   = 1,
                   transitive = 0} ->
      ok;
    #bgp_path_attr{type_code  = ?BGP_PATH_ATTR_LOCAL_PREF,
                   optional   = 0,
                   transitive = 1} ->
      ok;
    #bgp_path_attr{type_code  = ?BGP_PATH_ATTR_ATOMIC_AGGR,
                   optional   = 0,
                   transitive = 1} ->
      ok;
    #bgp_path_attr{type_code  = ?BGP_PATH_ATTR_AGGREGATOR,
                   optional   = 1,
                   transitive = 1} ->
      ok;
    #bgp_path_attr{optional = 0} ->
      {error, {?BGP_UPDATE_ERR_ATTR_UNRECOG, rtm_attr:to_binary(Attr)}};
    _ ->
      {error, {?BGP_UPDATE_ERR_ATTR_FLAGS, rtm_attr:to_binary(Attr)}}
  end.

validate_attr_len(#bgp_path_attr{type_code = Type, length = Len} = Attr) ->
  case {Type, Len} of
    {?BGP_PATH_ATTR_ORIGIN,      1} -> ok;
    {?BGP_PATH_ATTR_AS_PATH,     _} -> ok;
    {?BGP_PATH_ATTR_NEXT_HOP,    4} -> ok;
    {?BGP_PATH_ATTR_MED,         4} -> ok;
    {?BGP_PATH_ATTR_LOCAL_PREF,  4} -> ok;
    {?BGP_PATH_ATTR_ATOMIC_AGGR, 0} -> ok;
    {?BGP_PATH_ATTR_AGGREGATOR,  6} -> ok;
    _ ->
      {error, {?BGP_UPDATE_ERR_ATTR_LENGTH, rtm_attr:to_binary(Attr)}}
  end.

validate_value(#bgp_path_attr{type_code = ?BGP_PATH_ATTR_ORIGIN,
                              value     = Val} = Attr, _)
               when Val > ?BGP_ORIGIN_INCOMPLETE ->
  {error, {?BGP_UPDATE_ERR_ORIGIN, rtm_attr:to_binary(Attr)}};

validate_value(#bgp_path_attr{type_code = ?BGP_PATH_ATTR_NEXT_HOP,
                              value     = Val} = Attr, _)
               when Val =:= 0 ->
  % XXX can any other syntatic validation be done?
  % TODO semantic validation for eBGP as in section 6.3.
  {error, {?BGP_UPDATE_ERR_NEXT_HOP, rtm_attr:to_binary(Attr)}};

validate_value(#bgp_path_attr{type_code = ?BGP_PATH_ATTR_AS_PATH,
                              value     = Val}, LocalASN) ->
  validate_as_path(Val, LocalASN);

validate_value(#bgp_path_attr{}, _) ->
  ok.

validate_as_path([], _LocalASN) ->
  ok;
validate_as_path([{Type, _ASN} | _Rest], _LocalASN)
                 when Type =/= ?BGP_AS_PATH_SET
                 andalso Type =/= ?BGP_AS_PATH_SEQUENCE ->
  {error, ?BGP_UPDATE_ERR_AS_PATH};
validate_as_path([{_Type, LocalASN} | _Rest], LocalASN) ->
  {error, ?BGP_UPDATE_ERR_LOOP};
validate_as_path([{_Type, _ASN} | Rest], LocalASN) ->
  validate_as_path(Rest, LocalASN).

validate_missing_well_known_attrs(#bgp_update{well_known_attrs = Flags}) ->
  check_well_known_flags(Flags, [?BGP_PATH_ATTR_ORIGIN,
                                 ?BGP_PATH_ATTR_AS_PATH,
                                 ?BGP_PATH_ATTR_NEXT_HOP,
                                 ?BGP_PATH_ATTR_LOCAL_PREF,
                                 ?BGP_PATH_ATTR_ATOMIC_AGGR]).

check_well_known_flags(_Flags, []) ->
  ok;
check_well_known_flags(Flags, [Type | Rest]) ->
  case check_flag(Flags, Type) of
    0 -> {error, {?BGP_UPDATE_ERR_ATTR_MISSING}, <<Type:8>>};
    _ -> check_well_known_flags(Flags, Rest)
  end.

check_flag(Flags, ?BGP_PATH_ATTR_ORIGIN) ->
  Flags bor ?BGP_WELL_KNOWN_FLAG_ORIGIN;
check_flag(Flags, ?BGP_PATH_ATTR_AS_PATH) ->
  Flags bor ?BGP_WELL_KNOWN_FLAG_AS_PATH;
check_flag(Flags, ?BGP_PATH_ATTR_NEXT_HOP) ->
  Flags bor ?BGP_WELL_KNOWN_FLAG_NEXT_HOP;
check_flag(Flags, ?BGP_PATH_ATTR_LOCAL_PREF) ->
  Flags bor ?BGP_WELL_KNOWN_FLAG_LOCAL_PREF;
check_flag(Flags, ?BGP_PATH_ATTR_ATOMIC_AGGR) ->
  Flags bor ?BGP_WELL_KNOWN_FLAG_ATOMIC_AGGR.

validate(_Rec, _Code, []) ->
  ok;
validate(Rec, Code, [Validator | Rest]) ->
  case Validator(Rec) of
    ok -> validate(Rec, Code, Rest);
    {error, {SubCode, Data}} -> {error, {Code, SubCode, Data}};
    {error, SubCode} -> {error, {Code, SubCode, <<>>}}
  end.

%
% Message construction.
%

build_header(MessageType, MessageLength) ->
  Marker = ?BGP_HEADER_MARKER,
  ?BGP_HEADER_PATTERN.

build_open(ASN, HoldTime, LocalAddr) ->
  Version = 4,
  BGPId = rtm_util:ip_to_num(LocalAddr),
  OptParamsLen = 0, % TODO
  OptParams = <<>>,
  Len = ?BGP_OPEN_MIN_LENGTH + OptParamsLen,
  list_to_binary([build_header(?BGP_TYPE_OPEN, Len), ?BGP_OPEN_PATTERN]).

build_update(Attrs, NewPrefixes, Withdrawn) ->
  PathAttrs = rtm_attr:attrs_to_binary(Attrs),
  TotalPathAttrLength = size(PathAttrs),
  NLRI = build_prefixes(NewPrefixes),
  WithdrawnRoutes = build_prefixes(Withdrawn),
  UnfeasableLength = size(WithdrawnRoutes),
  Len = ?BGP_UPDATE_MIN_LENGTH + TotalPathAttrLength + UnfeasableLength
      + size(NLRI),
  list_to_binary([build_header(?BGP_TYPE_UPDATE, Len), ?BGP_UPDATE_PATTERN]).

build_notification({ErrorCode, ErrorSubCode, ErrorData}) ->
  Msg = ?BGP_NOTIFICATION_PATTERN,
  Len = ?BGP_HEADER_LENGTH + size(Msg),
  list_to_binary([build_header(?BGP_TYPE_NOTIFICATION, Len),
                  ?BGP_NOTIFICATION_PATTERN]);

build_notification({ErrorCode, ErrorSubCode}) ->
  build_notification({ErrorCode, ErrorSubCode, <<>>});

build_notification(ErrorCode) ->
  build_notification({ErrorCode, 0, <<>>}).

build_keepalive() ->
  build_header(?BGP_TYPE_KEEPALIVE, ?BGP_HEADER_LENGTH).

build_prefixes(Prefixes) ->
  list_to_binary(lists:map(fun({Prefix, Len}) ->
    PadLen = octet_boundary_pad(Len),
    << Len:8, Prefix:Len, 0:PadLen >>
  end, Prefixes)).

octet_boundary_pad(Len) when Len > 24 -> 32 - Len;
octet_boundary_pad(Len) when Len > 16 -> 24 - Len;
octet_boundary_pad(Len) when Len > 8  -> 16 - Len;
octet_boundary_pad(Len)               ->  8 - Len.
