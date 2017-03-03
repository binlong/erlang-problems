-module(json).
-export([render/1, proplist_to_json/2, generate_tuple_json/1]).

render(JsonBody) -> 
	{Properties} = JsonBody,
	JsonStringList = [generate_tuple_json(Property) || Property <- Properties],
	JsonString = string:join(JsonStringList, ","),
	"{" ++ JsonString ++ "}".

proplist_to_json(Key, Value) ->
	JsonKey = atom_to_list(Key),
	if is_binary(Value) ->
			JsonString = "\"" ++ JsonKey ++ "\":\"" ++ binary_to_list(Value) ++ "\"";
	   is_integer(Value) ->
	   		JsonString = "\"" ++ JsonKey ++ "\":" ++ integer_to_list(Value) ++ "";
	   is_list(Value) ->
	   		TupleList = [generate_tuple_json(X) || X <- Value, is_tuple(X)],
	   		BinaryList = [binary_to_list(X) || X <- Value, is_binary(X)],
	   		if length(BinaryList) > 0 ->
	   				FormattedJsonValues = ["\"" ++ JsonValue ++ "\"" || JsonValue <- BinaryList],
	   				JsonValueString = string:join(FormattedJsonValues, ","),
	   				JsonString = "\"" ++ JsonKey ++ "\":[" ++ JsonValueString ++ "]";
	   		   length(TupleList) > 0 ->
	   				JsonValueString = string:join(TupleList, ","),
	   				JsonString = "\"" ++ JsonKey ++ "\":[" ++ JsonValueString ++ "]"
   			end,
   			JsonString
	end,
	JsonString.

generate_tuple_json(Item) ->
	Size = tuple_size(Item),
	if Size =:= 2 ->
			{Key, Value} = Item,
			JsonString = proplist_to_json(Key, Value);
	   Size =:= 1 ->
	   		{Elements} = Item,
	   		JsonStringList = [generate_tuple_json(Element) || Element <- Elements],
	   		JsonValueString = string:join(JsonStringList, ","),
	   		JsonString = "{" ++ JsonValueString ++ "}"
	end,
	JsonString.
