-module(rtm_msg).
-include_lib("bgp.hrl").

-export([validate_header/1, validate_open/1, validate_update/2,
         validate_notification/1]).

%
% Validations.
%

validate_header(Hdr) ->
  validate(Hdr, [fun validate_marker/1,
                 fun validate_msg_len/1,
                 fun validate_type/1]).

validate_open(Msg) ->
  validate(Msg, [fun validate_version/1,
                 fun validate_asn/1,
                 fun validate_hold_time/1,
                 fun validate_bgp_id/1,
                 fun validate_opt_params/1]).

validate_update(Msg, MsgLen) ->
  validate(Msg, [fun(M) -> validate_update_length(M, MsgLen) end,
                 fun validate_path_attrs/1,
                 fun validate_missing_well_known_attrs/1]).

validate_notification(Msg) ->
  validate(Msg, [fun validate_error/1]).


%
% Internal functions.
%

% Header validation.

validate_marker(#bgp_header{msg_type = ?BGP_TYPE_OPEN,
                            marker = ?BGP_HEADER_MARKER}) ->
  ok;
validate_marker(#bgp_header{msg_type = ?BGP_TYPE_OPEN}) ->
  {error, {?BGP_ERR_HEADER, ?BGP_HEADER_ERR_SYNC}};
validate_marker(#bgp_header{}) ->
  ok. % TODO Handle auth.

validate_msg_len(#bgp_header{msg_type = ?BGP_TYPE_OPEN, msg_len = Len})
  when Len >= ?BGP_OPEN_MIN_LENGTH andalso Len =< ?BGP_MAX_MSG_LEN ->
    ok;
validate_msg_len(#bgp_header{msg_type = ?BGP_TYPE_UPDATE, msg_len = Len})
  when Len >= ?BGP_UPDATE_MIN_LENGTH andalso Len =< ?BGP_MAX_MSG_LEN ->
    ok;
validate_msg_len(#bgp_header{msg_type = ?BGP_TYPE_NOTIFICATION, msg_len = Len})
  when Len >= ?BGP_NOTIFICATION_MIN_LENGTH andalso Len =< ?BGP_MAX_MSG_LEN ->
    ok;
validate_msg_len(#bgp_header{msg_type = ?BGP_TYPE_KEEPALIVE, msg_len = Len})
  when Len =:= ?BGP_HEADER_LENGTH ->
    ok;
validate_msg_len(#bgp_header{msg_len = Len}) ->
  {error, {?BGP_ERR_HEADER, ?BGP_HEADER_ERR_LENGTH, <<Len:8>>}}.

validate_type(#bgp_header{msg_type = ?BGP_TYPE_OPEN})         -> ok;
validate_type(#bgp_header{msg_type = ?BGP_TYPE_UPDATE})       -> ok;
validate_type(#bgp_header{msg_type = ?BGP_TYPE_NOTIFICATION}) -> ok;
validate_type(#bgp_header{msg_type = ?BGP_TYPE_KEEPALIVE})    -> ok;
validate_type(#bgp_header{msg_type = Type}) ->
  {error, {?BGP_ERR_HEADER, ?BGP_HEADER_ERR_TYPE, <<Type:8>>}}.

% OPEN message validation.

validate_version(#bgp_open{version = 4}) ->
  ok;
validate_version(#bgp_open{}) ->
  {error, {?BGP_ERR_OPEN, ?BGP_OPEN_ERR_VERSION, <<4:16>>}}.

% TODO
validate_asn(#bgp_open{}) ->
  ok.

validate_hold_time(#bgp_open{hold_time = HoldTime}) when HoldTime < 3 ->
  {error, {?BGP_ERR_OPEN, ?BGP_OPEN_ERR_PEER_AS}};
validate_hold_time(#bgp_open{}) ->
  ok.

% XXX any other possible validation?
validate_bgp_id(#bgp_open{bgp_id = Id}) ->
  case Id of
    0 -> {error, {?BGP_ERR_OPEN, ?BGP_OPEN_ERR_BGP_ID}};
    _ -> ok
  end.

% TODO
validate_opt_params(#bgp_open{}) ->
  ok.


% UPDATE message validation.
% TODO optional attributes; NLRI.

validate_update_length(#bgp_update{unfeasible_len = ULen, attrs_len = ALen},
                       MsgLen) ->
  case ?BGP_UPDATE_MIN_LENGTH + ULen + ALen of
    MsgLen -> ok;
    _      -> {error, {?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_LIST}}
  end.

validate_path_attrs(#bgp_update{path_attrs = PathAttrs}) ->
  validate_attrs(PathAttrs, dict:new()).

validate_attrs([], AttrCounters) ->
  Dups = dict:filter(fun(_Type, Count) -> Count > 1 end, AttrCounters),
  case size(Dups) of
    0 -> ok;
    _ -> {error, {?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_LIST}}
  end;
validate_attrs([#bgp_path_attr{type_code = T} = Attr | Rest], AttrCounters) ->
  case validate_attr(Attr) of
    ok -> validate_attrs(Rest, dict:update_counter(T, 1, AttrCounters));
    {error, Subcode, Data} -> {error, ?BGP_ERR_UPDATE, Subcode, Data}
  end.

validate_attr(Attr) ->
  validate(Attr, [fun validate_flags/1,
                  fun validate_attr_len/1,
                  fun validate_value/1]).

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
      {error, ?BGP_UPDATE_ERR_ATTR_UNRECOG, build_attr(Attr)};
    _ ->
      {error, ?BGP_UPDATE_ERR_ATTR_FLAGS, build_attr(Attr)}
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
      {error, ?BGP_UPDATE_ERR_ATTR_LENGTH, build_attr(Attr)}
  end.

% TODO validate AS_PATH values. All syntatic validation should
% have been done in rtm_parser, so the error should probably
% be caught elsewhere anyway.
validate_value(#bgp_path_attr{type_code = ?BGP_PATH_ATTR_ORIGIN,
                              value     = Val} = Attr)
               when Val > ?BGP_PATH_ATTR_ORIGIN_INCOMPLETE ->
  {error, ?BGP_UPDATE_ERR_ORIGIN, build_attr(Attr)};

validate_value(#bgp_path_attr{type_code = ?BGP_PATH_ATTR_NEXT_HOP,
                              value     = Val} = Attr)
               when Val =:= 0 ->
  % XXX can any other syntatic validation be done?
  % TODO semantic validation for eBGP as in section 6.3.
  {error, ?BGP_UPDATE_ERR_NEXT_HOP, build_attr(Attr)}.


build_attr(#bgp_path_attr{optional   = Opt,
                          transitive = Trans,
                          partial    = Partial,
                          extended   = Ext,
                          type_code  = Type,
                          length     = Len,
                          raw_value  = Val}) ->
  LenSize = (Ext + 1) * 8,
  <<Opt:1,Trans:1,Partial:1,Ext:1,0:8,Type:8,Len:LenSize,Val>>.

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
    0 -> {error, {?BGP_ERR_UPDATE, ?BGP_UPDATE_ERR_ATTR_MISSING}, <<Type:8>>};
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


% NOTIFICATION message validation.

validate_error(#bgp_notification{error_code = ?BGP_ERR_HEADER} = Msg) ->
  validate_header_error(Msg#bgp_notification.error_subcode);
validate_error(#bgp_notification{error_code = ?BGP_ERR_OPEN} = Msg) ->
  validate_open_error(Msg#bgp_notification.error_subcode);
validate_error(#bgp_notification{error_code = ?BGP_ERR_UPDATE} = Msg) ->
  validate_update_error(Msg#bgp_notification.error_subcode);
validate_error(#bgp_notification{error_code = ?BGP_ERR_HOLD_TIME}) ->
  ok;
validate_error(#bgp_notification{error_code = ?BGP_ERR_FSM}) ->
  ok;
validate_error(#bgp_notification{error_code = ?BGP_ERR_CEASE}) ->
  ok;
validate_error(#bgp_notification{error_code = C, error_subcode = S}) ->
  invalid_error(C, S).

validate_header_error(?BGP_HEADER_ERR_SYNC)   -> ok;
validate_header_error(?BGP_HEADER_ERR_LENGTH) -> ok;
validate_header_error(?BGP_HEADER_ERR_TYPE)   -> ok;
validate_header_error(Subcode) ->
  invalid_error(?BGP_ERR_OPEN, Subcode).

validate_open_error(?BGP_OPEN_ERR_VERSION)   -> ok;
validate_open_error(?BGP_OPEN_ERR_PEER_AS)   -> ok;
validate_open_error(?BGP_OPEN_ERR_BGP_ID)    -> ok;
validate_open_error(?BGP_OPEN_ERR_OPT_PARAM) -> ok;
validate_open_error(?BGP_OPEN_ERR_AUTH_FAIL) -> ok;
validate_open_error(?BGP_OPEN_ERR_HOLD_TIME) -> ok;
validate_open_error(Subcode) ->
  invalid_error(?BGP_ERR_OPEN, Subcode).

validate_update_error(?BGP_UPDATE_ERR_ATTR_LIST)    -> ok;
validate_update_error(?BGP_UPDATE_ERR_ATTR_UNRECOG) -> ok;
validate_update_error(?BGP_UPDATE_ERR_ATTR_MISSING) -> ok;
validate_update_error(?BGP_UPDATE_ERR_ATTR_FLAGS)   -> ok;
validate_update_error(?BGP_UPDATE_ERR_ATTR_LENGTH)  -> ok;
validate_update_error(?BGP_UPDATE_ERR_ORIGIN)       -> ok;
validate_update_error(?BGP_UPDATE_ERR_LOOP)         -> ok;
validate_update_error(?BGP_UPDATE_ERR_NEXT_HOP)     -> ok;
validate_update_error(?BGP_UPDATE_ERR_OPT_ATTR)     -> ok;
validate_update_error(?BGP_UPDATE_ERR_NETWORK)      -> ok;
validate_update_error(?BGP_UPDATE_ERR_AS_PATH)      -> ok;
validate_update_error(Subcode) ->
  invalid_error(?BGP_ERR_OPEN, Subcode).

invalid_error(Code, Subcode) ->
  io:format("Invalid error code ~p, subcode ~p~n", [Code, Subcode]).

validate(_Rec, []) ->
  ok;
validate(Rec, [Validator | Rest]) ->
  case Validator(Rec) of
    ok -> validate(Rec, Rest);
    {error, Error} -> {error, Error}
  end.
