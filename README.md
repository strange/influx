# Erlang driver for InfluxDB

## About

This is a rudimentary version I created in order to take InfluxDB for a spin.
It's not ready for use.

## Usage

Initialize a connection:

    {ok, DB} = influx:connect(<<"mydb">>).

Write data:

    ok = influx:write(DB, cpu, #{usage => 0.4}).

With tags:

    ok = influx:write(DB, cpu, #{hostname => dark_aster} , #{value => 0.01}).

Make queries:

    {ok, Results} = influx:query(DB, "SELECT * FROM cpu").
    maps:get(<<"columns">>, Results).
    maps:get(<<"values">>, Results).

## TODO

- Multiple queries
- Multiple writes
- Input validation
- Lots more
