## counter

An OTP application

---

## Build

    $ ./rebar3 compile

---

## Common Tests

    $ ./rebar3 ct

---

## Use

* type in console `./rebar3 shell` to run application
* use `counter_srv:incr()` to increase call counter
* use `counter_srv:get_value()` to get number of call counters for the last  **60 seconds**.

---

## Deps

Erlang OTP 25

