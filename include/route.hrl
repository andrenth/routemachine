-ifndef(ROUTES_HRL).
-define(ROUTES_HRL, true).

-include_lib("types.hrl").
-include_lib("bgp.hrl").

-record(route_attrs, {
  active       :: boolean(),
  next_hop     :: uint32(),
  path_attrs   :: bgp_path_attrs(),
  ebgp         :: boolean(),
  as_path_loop :: boolean(),
  peer_addr    :: ipv4_address(),
  peer_bgp_id  :: ipv4_address()
}).

-endif.
