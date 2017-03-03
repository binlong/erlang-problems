-module(json_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

render_test() ->
  {ok, Data} = file:read_file("tests/bob.json"),
  Expected = binary_to_list(Data),
  Rendered = json:render({[
    {name, <<"Bob">>},
    {age, 192},
    {phone_numbers, [<<"123-456-7890">>, <<"890-567-1234">>]},
    {children, [
      {[{name, <<"Fred">>},
        {age, 12}]},
      {[{name, <<"Jane">>},
        {age, 8},
        {hobbies, [<<"Reading">>, <<"Swimming">>, <<"Skating">>]}]}]}]}),
  io:format("~nExpected: ~p", [Expected]),
  io:format("~nRendered: ~p", [Rendered]),
  ?assertEqual(Expected, Rendered).


proplist_to_json_binary_test() ->
  Expected = "\"name\":\"Bob\"",
  Rendered = json:proplist_to_json(name, <<"Bob">>),
  io:format("~nExpected: ~p", [Expected]),
  io:format("~nRendered: ~p", [Rendered]),
  ?assertEqual(Expected, Rendered).

proplist_to_json_integer_test() ->
  Expected = "\"age\":192",
  Rendered = json:proplist_to_json(age, 192),
  io:format("~nExpected: ~p", [Expected]),
  io:format("~nRendered: ~p", [Rendered]),
  ?assertEqual(Expected, Rendered).

proplist_to_json_list_binary_test() ->
  Expected = "\"phone_numbers\":[\"123-456-7890\",\"890-567-1234\"]",
  Rendered = json:proplist_to_json(phone_numbers, [<<"123-456-7890">>, <<"890-567-1234">>]),
  io:format("~nExpected: ~p", [Expected]),
  io:format("~nRendered: ~p", [Rendered]),
  ?assertEqual(Expected, Rendered).

proplist_to_json_nested_list_test() ->
  Expected = "\"children\":[{\"name\":\"Fred\",\"age\":12},{\"name\":\"Jane\",\"age\":8,\"hobbies\":[\"Reading\",\"Swimming\",\"Skating\"]}]",
  Rendered = json:proplist_to_json(children, [{[{name, <<"Fred">>},{age, 12}]},{[{name, <<"Jane">>},{age, 8},{hobbies, [<<"Reading">>, <<"Swimming">>, <<"Skating">>]}]}]),
  io:format("~nExpected: ~p", [Expected]),
  io:format("~nRendered: ~p", [Rendered]),
  ?assertEqual(Expected, Rendered).

generate_tuple_json_size_two_test() ->
  Expected = "\"name\":\"Bob\"",
  Rendered = json:generate_tuple_json({name, <<"Bob">>}),
  io:format("~nExpected: ~p", [Expected]),
  io:format("~nRendered: ~p", [Rendered]),
  ?assertEqual(Expected, Rendered).

generate_tuple_json_size_one_test() ->
  Expected = "{{\"name\":\"Fred\",\"age\":12},{\"name\":\"Jane\",\"age\":8,\"hobbies\":[\"Reading\",\"Swimming\",\"Skating\"]}}",
  Rendered = json:generate_tuple_json({[{[{name, <<"Fred">>},{age, 12}]},{[{name, <<"Jane">>},{age, 8},{hobbies, [<<"Reading">>, <<"Swimming">>, <<"Skating">>]}]}]}),
  io:format("~nExpected: ~p", [Expected]),
  io:format("~nRendered: ~p", [Rendered]),
  ?assertEqual(Expected, Rendered).