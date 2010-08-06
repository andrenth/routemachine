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
