-module(influx).

-export([start/0]).

-export([connect/1]).
-export([connect/2]).

-export([write/3]).
-export([write/4]).
-export([write/5]).
-export([query/2]).

-record(db, {
    name :: binary(),
    url = <<"127.0.0.1:8086">> :: binary()
 }).

start() ->
    application:ensure_all_started(?MODULE).

connect(Name) ->
    {ok, #db{ name = Name }}.
connect(Name, URL) ->
    {ok, #db{ name = Name, url = URL }}.

write(DB, Measurement, Fields) ->
    write(DB, Measurement, #{}, Fields, undefined).

write(DB, Measurement, Tags, Fields) ->
    write(DB, Measurement, Tags, Fields, undefined).

write(DB, Measurement, Tags, Fields, Timestamp) ->
    URL = make_url(DB, write),
    Packet = influx_line_protocol:make_packet(Measurement, Tags, Fields,
                                              Timestamp),
    handle_response(hackney:post(URL, [], Packet)).

%% @doc Make a query
query(DB, Query) ->
    URL = make_url(DB, query, [{<<"q">>, Query}]),
    handle_response(hackney:get(URL, [])).

%% @doc Handle response from Influx
handle_response({ok, 200, _Headers, Ref}) ->
    {ok, Body} = hackney:body(Ref),
    %% TODO: Support multiple series
    Results = hd(maps:get(<<"results">>, jsx:decode(Body, [return_maps]))),
    Series = hd(maps:get(<<"series">>, Results)),
    {ok, Series};
handle_response({ok, 204, _Headers, _Ref}) ->
    ok;
handle_response({ok, _Code, Headers, Ref}) ->
    {ok, Body} = hackney:body(Ref),
    case lists:keyfind(<<"Content-Type">>, 1, Headers) of
        {_, <<"application/json">>} ->
            Reason = maps:get(<<"error">>, jsx:decode(Body, [return_maps])),
            {error, Reason};
        _ ->
            {error, Body}
    end;
handle_response({error, Reason}) ->
    {error, Reason}.

%% @doc Create a URL for request
make_url(DB, Type) ->
    make_url(DB, Type, []).

make_url(DB, write, Qs) ->
    make_url(DB, <<"write">>, Qs);
make_url(DB, query, Qs) ->
    make_url(DB, <<"query">>, Qs);
make_url(#db{name = Name, url = URL}, Type, Qs) ->
    NewQs = [{<<"db">>, Name}|Qs],
    hackney_url:make_url(URL, Type, NewQs).
