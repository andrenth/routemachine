-ifndef(TYPES_HRL).
-define(TYPES_HRL, true).

-type uint16()       :: 0..65535.
-type uint32()       :: 0..4294967295.
-type ipv4_address() :: {byte(), byte(), byte(), byte()}.
-type prefix()       :: uint32().
-type prefix_len()   :: 0..32.
-type prefix_list()  :: [{prefix(), prefix_len()}].
-type rib()          :: dict().

-endif.
