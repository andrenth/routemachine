-record(session, {
  establishment,
  server,
  local_asn,
  peer_asn,
  local_addr,
  peer_addr,
  peer_bgp_id,
  networks,
  hold_time,
  hold_timer,
  keepalive_time,
  keepalive_timer,
  conn_retry_time,
  conn_retry_timer,
  idle_time
}).

-type session() :: #session{}.
