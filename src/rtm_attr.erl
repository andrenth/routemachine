-module(rtm_attr).
-export([get/2, fold/3, from_list/1]).
-export([attrs_to_binary/1, to_binary/1]).
-export([origin/1, as_path/1, next_hop/1]).
-export([update_for_ebgp/5, update_for_ibgp/3]).

-include_lib("bgp.hrl").

get(Attr, PathAttrs) ->
  [#bgp_path_attr{value = Value}] = dict:fetch(Attr, PathAttrs),
  Value.

attrs_to_binary(Attrs) ->
  AttrList =
    dict:fold(fun(_Type, [#bgp_path_attr{extended = Ext} = Attr], Acc) ->
      {Type, Len, Val} = to_binary(Attr),
      L = (Ext + 1) * 8,
      [<< Type:16,Len:L,Val:Len/binary >> | Acc]
    end, [], Attrs),
  list_to_binary(AttrList).

to_binary(#bgp_path_attr{optional   = Opt,
                         transitive = Trans,
                         partial    = Partial,
                         extended   = Ext,
                         type_code  = TypeCode,
                         length     = Len,
                         raw_value  = Val}) ->
  <<Type:16>> = <<Opt:1,Trans:1,Partial:1,Ext:1,0:4,TypeCode:8>>,
  {Type, Len, Val}.

prepend_asn(ASN, #bgp_path_attr{extended  = Extended,
                                type_code = ?BGP_PATH_ATTR_AS_PATH,
                                raw_value = Path} = ASPath) ->
  NewPath = case Path of
    << ?BGP_AS_PATH_SEQUENCE:8, N:8, FirstASN:16, Rest/binary >> ->
      << ?BGP_AS_PATH_SEQUENCE:8, (N+1):8, ASN:16, FirstASN:16, Rest/binary >>;
    << ?BGP_AS_PATH_SET:8, _Rest/binary >> ->
      << ?BGP_AS_PATH_SEQUENCE:8, 1:8, ASN:16, Path/binary >>;
    << >> ->
      << ?BGP_AS_PATH_SEQUENCE:8, 1:8, ASN:16 >>
  end,
  Length = size(NewPath),
  ASPath#bgp_path_attr{
    extended  = case Extended of 0 -> extended_bit(Length); 1 -> 1 end,
    length    = Length,
    raw_value = NewPath
  }.

origin(Origin) ->
  build(?BGP_PATH_ATTR_ORIGIN, <<Origin:8>>, [transitive]).

as_path(Path) ->
  build(?BGP_PATH_ATTR_AS_PATH, Path, [transitive]).

next_hop(Addr) ->
  Bin = <<(rtm_util:ip_to_num(Addr)):32>>,
  build(?BGP_PATH_ATTR_NEXT_HOP, Bin, [transitive]).

update_for_ebgp(TypeCode, Attr, PathAttrs, ASN, Addr) ->
  case TypeCode of
    ?BGP_PATH_ATTR_AS_PATH ->
      NewASPath = prepend_asn(ASN, Attr),
      dict:store(?BGP_PATH_ATTR_AS_PATH, [NewASPath], PathAttrs);
    ?BGP_PATH_ATTR_NEXT_HOP ->
      NewNextHop = rtm_attr:next_hop(Addr),
      dict:store(?BGP_PATH_ATTR_NEXT_HOP, [NewNextHop], PathAttrs);
    _ ->
      PathAttrs
  end.

update_for_ibgp(TypeCode, Attr, PathAttrs) ->
  case TypeCode of
    ?BGP_PATH_ATTR_MED ->
      dict:store(?BGP_PATH_ATTR_MED, [Attr], PathAttrs);
    ?BGP_PATH_ATTR_LOCAL_PREF ->
      dict:store(?BGP_PATH_ATTR_LOCAL_PREF, [Attr], PathAttrs);
    _ ->
      PathAttrs
  end.

build(TypeCode, Bin, Flags) ->
  Length = size(Bin),
  InitAttr = #bgp_path_attr{
    optional   = 0,
    transitive = 0,
    partial    = 0,
    extended   = extended_bit(Length),
    type_code  = TypeCode,
    length     = Length,
    raw_value  = Bin
  },
  lists:foldl(fun(Flag, Acc) ->
    case Flag of
      optional   -> Acc#bgp_path_attr{optional   = 1};
      transitive -> Acc#bgp_path_attr{transitive = 1};
      partial    -> Acc#bgp_path_attr{partial    = 1};
      extended   -> Acc#bgp_path_attr{extended   = 1}
    end
  end, InitAttr, Flags).

extended_bit(Length) ->
  case Length > 255 of
    true  -> 1;
    false -> 0
  end.

fold(Fun, Init, Attrs) ->
  dict:fold(fun(TypeCode, [Attr], Acc) ->
    Fun(TypeCode, Attr, Acc)
  end, Init, Attrs).

from_list(Attrs) ->
  from_list(Attrs, dict:new()).

from_list([], Attrs) ->
  Attrs;
from_list([{TypeCode, Attr}| Rest], Attrs) ->
  from_list(Rest, dict:store(TypeCode, [Attr], Attrs)).
