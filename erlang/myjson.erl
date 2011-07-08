-module(myjson).
-export([encode/3]).

encode(RiakObject, _, _) ->
  Term = binary_to_term(riak_object:get_value(RiakObject)),
  [tl(tuple_to_list(Term))].
