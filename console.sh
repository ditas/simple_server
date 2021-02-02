#!/bin/bash

rebar3 release

./_build/default/rel/simple_server/bin/simple_server console
