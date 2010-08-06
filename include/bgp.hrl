% Lengths.
-define(BGP_HEADER_LENGTH,    19).
-define(BGP_MAX_MSG_LEN,    4096).

% Message types.
-define(BGP_TYPE_OPEN,         1).
-define(BGP_TYPE_UPDATE,       2).
-define(BGP_TYPE_NOTIFICATION, 3).
-define(BGP_TYPE_KEEPALIVE,    4).

% Optional parameters.
-define(BGP_PARAM_AUTH_INFO,   1).

% Path attributes.
-define(BGP_PATH_ATTR_ORIGIN,      1).
-define(BGP_PATH_ATTR_AS_PATH,     2).
-define(BGP_PATH_ATTR_NEXT_HOP,    3).
-define(BGP_PATH_ATTR_MED,         4).
-define(BGP_PATH_ATTR_LOCAL_PREF,  5).
-define(BGP_PATH_ATTR_ATOMIC_AGGR, 6).
-define(BGP_PATH_ATTR_AGGREGATOR,  7).

% Path attribute values.
-define(BGP_PATH_ATTR_ORIGIN_IGP,        0).
-define(BGP_PATH_ATTR_ORIGIN_EGP,        1).
-define(BGP_PATH_ATTR_ORIGIN_INCOMPLETE, 2).

% Error codes
-define(BGP_ERR_HEADER,    1).
-define(BGP_ERR_OPEN,      2).
-define(BGP_ERR_UPDATE,    3).
-define(BGP_ERR_HOLD_TIME, 4).
-define(BGP_ERR_FSM,       5).
-define(BGP_ERR_CEASE,     6).

% Header error subcodes.
-define(BGP_HEADER_ERR_SYNC,   1).
-define(BGP_HEADER_ERR_LENGTH, 2).
-define(BGP_HEADER_ERR_TYPE,   3).

% OPEN error subcodes.
-define(BGP_OPEN_ERR_VERSION,   1).
-define(BGP_OPEN_ERR_PEER_AS,   2).
-define(BGP_OPEN_ERR_BGP_ID,    3).
-define(BGP_OPEN_ERR_OPT_PARAM, 4).
-define(BGP_OPEN_ERR_AUTH_FAIL, 5).
-define(BGP_OPEN_ERR_HOLD_TIME, 6).

% UPDATE error subcodes.
-define(BGP_UPDATE_ERR_ATTR_LIST,    1).
-define(BGP_UPDATE_ERR_ATTR_UNRECOG, 2).
-define(BGP_UPDATE_ERR_ATTR_MISSING, 3).
-define(BGP_UPDATE_ERR_ATTR_FLAGS,   4).
-define(BGP_UPDATE_ERR_ATTR_LENGTH,  5).
-define(BGP_UPDATE_ERR_ORIGIN,       6).
-define(BGP_UPDATE_ERR_ROUTING_LOOP, 7).
-define(BGP_UPDATE_ERR_NEXT_HOP,     8).
-define(BGP_UPDATE_ERR_OPT_ATTR,     9).
-define(BGP_UPDATE_ERR_NETWORK,      10).
-define(BGP_UPDATE_ERR_AS_PATH,      11).

%
% Messages
%

-record(bgp_header, {
  message_length,
  message_type
}).

-record(bgp_open, {
  version,
  asn,
  hold_time,
  bgp_id,
  opt_params_len,
  opt_params
}).

-record(bgp_auth_info, {
  code,
  data
}).

-record(bgp_update,{
    withdrawn_routes,
    path_attrs,
    nlri
}).

-record(bgp_path_attr, {
  optional,
  transitive,
  partial,
  type_code,
  length,
  value
}).
