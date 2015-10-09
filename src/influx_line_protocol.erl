-module(influx_line_protocol).

-export([make_packet/4]).

-define(SEP(V), case V of <<>> -> <<>>; [] -> <<>>; _ -> <<$,>> end).

%% @doc Convert and escape a value
value(V) when is_integer(V) ->
    [integer_to_binary(V), $i];
value(V) when is_float(V) ->
    float_to_binary(V);
value(V) when is_atom(V) ->
    value(atom_to_binary(V, utf8));
value(V) when is_list(V) ->
    value(list_to_binary(V));
value(V) when is_binary(V) ->
    [$", binary:replace(V, <<$">>, <<$\\, $">>, [global]), $"].

%% @doc Convert and validate a key (measurement, tag key, tag value, or field
%% key)
key(K) when is_atom(K) -> 
    key(atom_to_binary(K, utf8));
key(K) -> 
    Options = [global, {insert_replaced, 1}],
    binary:replace(K, [<<" ">>, <<$,>>, <<$=>>], <<$\\>>, Options).

%% @doc Flatten and escape fields and values
flatten_fields(Fields) ->
    maps:fold(fun(K, V, Acc) ->
        [Acc, ?SEP(Acc), key(K), $=, value(V)]
    end, <<>>, Fields).

%% @doc Sort, flatten and escape tags
flatten_tags(Tags) when is_map(Tags) ->
    flatten_tags(maps:to_list(Tags));
flatten_tags(Tags) ->
    lists:foldl(fun({K, V}, Acc) ->
        [Acc, ?SEP(Acc), key(K), $=, V]
    end, [], lists:keysort(1, Tags)).

%% @doc Return optional timestamp
make_timestamp(undefined) -> <<>>;
make_timestamp(Timestamp) -> [" ", integer_to_binary(Timestamp)].

%% @doc Create a line protocol packet
make_packet(Measurement, Tags, Fields, Timestamp) ->
    BinaryTags = flatten_tags(Tags),
    BinaryFields = flatten_fields(Fields),
    BinaryTimestamp = make_timestamp(Timestamp),
    [key(Measurement), ?SEP(BinaryTags), BinaryTags, " ", BinaryFields,
     BinaryTimestamp].
