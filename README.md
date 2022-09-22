## counter

An OTP application

---

## Build

    $ rebar3 compile

---

## Use

* type in console `./rebar3 shell` to run application
* use `counter:incr()` to increase call counter
* use `counter:get_value()` to get number of call counters for the last  **60 seconds**.

---

## Deps

Erlang OTP 25

