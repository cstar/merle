#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    merle:connect("ketama.servers"),
    merle:set("toto", toto),
    merle:get("toto").