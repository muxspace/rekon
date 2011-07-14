-module(myjson).
-export([encode/3, encode/1]).

encode(RiakObject, _, _) ->
  io:format("~p", [RiakObject]),
  encode(riak_object:get_value(RiakObject)).

encode(<<"{}">>) -> [];
encode({M,S,N}) -> [M,S,N];
encode(Other) ->
  Term = binary_to_term(Other),
  [tl(tuple_to_list(Term))].

