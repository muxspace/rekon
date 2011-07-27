%% Provides some common data massaging functions
-module(masseuse).
-export([as_keydata/2, binary_to_term/3]).

%% As a reduce job
as_keydata(List,_) ->
  lists:map(fun(X) -> {{none,none}, X} end, List).

%% Map job to prepare data for a reduce job
binary_to_term(RiakObject, _, _) ->
  Raw = riak_object:get_value(RiakObject),
  case is_binary(Raw) of
    true -> Term = binary_to_term(Raw);
    false -> Term = Raw
  end,
  [ Term ].

