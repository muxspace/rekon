%% This module formats erlang terms to a human-readable string
-module(erl_viewer).
-export([to_string/2, to_string/3, to_string/1]).

%% As a reduce job
to_string(List, _) -> to_string(List).

%% Client:mapred([{<<"test_2">>,<<"1">>}], 
%%   [{map, {modfun, erl_viewer,to_string}, none, true}]).
to_string(RiakObject, _, _) ->
  O = riak_object:get_value(RiakObject),
  %io:format("[erl_viewer] Converting object~n  ~p~n", [O]),
  to_string(O).

to_string(<<"{}">>) -> [];
to_string({M,S,N}) -> [M,S,N];
to_string(Term) when is_binary(Term) -> to_string(binary_to_term(Term));
to_string(Term) ->
  Out = lists:flatten(io_lib:format("~p", [Term])),
  io:format("[erl_viewer] Flattened to~n  ~p~n", [Out]),
  [ list_to_binary(Out) ].

