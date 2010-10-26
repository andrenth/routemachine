-include_lib("types.hrl").

-record(session, {
  establishment    :: active | {passive, port()},
  server           :: pid(),
  local_asn        :: uint16(),
  peer_asn         :: uint16(),
  local_addr       :: ipv4_address(),
  peer_addr        :: ipv4_address(),
  peer_bgp_id      :: ipv4_address(),
  networks         :: [{ipv4_address(), prefix_len()}],
  hold_time        :: non_neg_integer(),
  hold_timer       :: reference(),
  keepalive_time   :: non_neg_integer(),
  keepalive_timer  :: reference(),
  conn_retry_time  :: non_neg_integer(),
  conn_retry_timer :: reference(),
  idle_time        :: non_neg_integer()
}).
