-ifndef(ROUTES_HRL).
-define(ROUTES_HRL, true).

-include_lib("types.hrl").
-include_lib("bgp.hrl").

-record(route, {
  active      :: boolean(),
  next_hop    :: uint32(),
  path_attrs  :: bgp_path_attrs(),
  ebgp        :: boolean(),
  bgp_id      :: ipv4_address(),
  peer_addr   :: ipv4_address(),
  fsm         :: pid()
}).

-type route() :: #route{}.

-endif.
