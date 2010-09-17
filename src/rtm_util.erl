-module(rtm_util).
-include_lib("bgp.hrl").

-export([ip_to_num/1, num_to_ip/1, error_string/3]).

ip_to_num({B1, B2, B3, B4}) ->
  (B1 bsl 24) bor (B2 bsl 16) bor (B3 bsl 8) bor B4.

num_to_ip(Num) ->
  B1 = (Num band 16#ff000000) bsr 24,
  B2 = (Num band 16#00ff0000) bsr 16,
  B3 = (Num band 16#0000ff00) bsr 8,
  B4 = (Num band 16#000000ff),
  {B1, B2, B3, B4}.

error_string(?BGP_ERR_HEADER, SubCode, Data) ->
  "Header error: " ++ header_error(SubCode, Data);

error_string(?BGP_ERR_OPEN, SubCode, Data) ->
  "OPEN message error: " ++ open_error(SubCode, Data);

error_string(?BGP_ERR_UPDATE, SubCode, Data) ->
  "UPDATE message error: " ++ update_error(SubCode, Data);

error_string(?BGP_ERR_HOLD_TIME, _SubCode, <<>>) ->
  "Hold timer expired";

error_string(?BGP_ERR_FSM, _SubCode, <<>>) ->
  "Finite State Machine error";

error_string(?BGP_ERR_CEASE, _SubCode, <<>>) ->
  "Cease";

error_string(Code, SubCode, Data) ->
  io_lib:format("Unknown error: ~B/~B/~w",
                [Code, SubCode, Data]).

header_error(?BGP_HEADER_ERR_SYNC, <<>>) ->
  "connection not synchronized";
header_error(?BGP_HEADER_ERR_LENGTH, <<BadLen:16>>) ->
  io_lib:format("bad message length: ~B", [BadLen]);
header_error(?BGP_HEADER_ERR_TYPE, <<BadType:8>>) ->
  io_lib:format("bad message type: ~B", [BadType]);
header_error(SubCode, Data) ->
  subcode_error(SubCode, Data).

open_error(?BGP_OPEN_ERR_VERSION, <<MaxVer:16>>) ->
  io_lib:format("unsupported version number: ~B", [MaxVer]);
open_error(?BGP_OPEN_ERR_PEER_AS, <<>>) ->
  "bad peer AS";
open_error(?BGP_OPEN_ERR_HOLD_TIME, <<>>) ->
  "unacceptable hold time";
open_error(?BGP_OPEN_ERR_BGP_ID, <<>>) ->
  "bad BGP identifier";
open_error(?BGP_OPEN_ERR_OPT_PARAM, <<>>) ->
  "unsupported optional parameter";
open_error(?BGP_OPEN_ERR_AUTH_FAIL, <<>>) ->
  "authentication failure";
open_error(SubCode, Data) ->
  subcode_error(SubCode, Data).

update_error(?BGP_UPDATE_ERR_ATTR_LIST, <<>>) ->
  "malformed attribute list";
update_error(?BGP_UPDATE_ERR_ATTR_FLAGS, Data) ->
  {Type, Length, Value} = parse_attr(Data),
  io_lib:format("attribute flags error: ~B/~B/~w", [Type, Length, Value]);
update_error(?BGP_UPDATE_ERR_ATTR_LENGTH, Data) ->
  Size = bit_size(Data),
  <<BadLen:Size>> = Data,
  io_lib:format("attribute length error: ~B", [BadLen]);
update_error(?BGP_UPDATE_ERR_ATTR_MISSING, <<Code:8>>) ->
  io_lib:format("missing well-known attribute: ~B", [Code]);
update_error(?BGP_UPDATE_ERR_ATTR_UNRECOG, Data) ->
  {Type, Length, Value} = parse_attr(Data),
  io_lib:format("unrecognized well-known attribute: ~B/~B/~w",
                [Type, Length, Value]);
update_error(?BGP_UPDATE_ERR_ORIGIN, Data) ->
  {Type, Length, Value} = parse_attr(Data),
  io_lib:format("invalid ORIGIN attribute: ~B/~B/~w", [Type, Length, Value]);
update_error(?BGP_UPDATE_ERR_NEXT_HOP, Data) ->
  {Type, Length, Value} = parse_attr(Data),
  io_lib:format("invalid NEXT_HOP attribute: ~B/~B/~w", [Type, Length, Value]);
update_error(?BGP_UPDATE_ERR_AS_PATH, <<>>) ->
  "malformed AS_PATH";
update_error(?BGP_UPDATE_ERR_OPT_ATTR, Data) ->
  {Type, Length, Value} = parse_attr(Data),
  io_lib:format("optional attribute error: ~B/~B/~w", [Type, Length, Value]);
update_error(?BGP_UPDATE_ERR_NETWORK, <<>>) ->
  "invalid network field";
update_error(SubCode, Data) ->
  subcode_error(SubCode, Data).

subcode_error(SubCode, Data) ->
  io_lib:format("unknown: subcode=~B, data=~w", [SubCode, Data]).

parse_attr(Data) ->
  << Type:8, Rest/binary >> = Data,
  << _:1, _:1, _:1, Extended:1, _:4 >> = Type,
  L = (Extended + 1) * 8,
  << _:8, Length:L, Value:Length/binary >> = Rest,
  {Type, Length, Value}.
