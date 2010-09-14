-module(rtm_util).

-export([ip_to_num/1, num_to_ip/1]).

ip_to_num({B1, B2, B3, B4}) ->
  (B1 bsl 24) bor (B2 bsl 16) bor (B3 bsl 8) bor B4.

num_to_ip(Num) ->
  B1 = (Num band 16#ff000000) bsr 24,
  B2 = (Num band 16#00ff0000) bsr 16,
  B3 = (Num band 16#0000ff00) bsr 8,
  B4 = (Num band 16#000000ff),
  {B1, B2, B3, B4}.
