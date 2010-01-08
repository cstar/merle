-module(mk).

-export([test/0, test2/0]).

test()->
    merle:connect("ketama.servers"),
    TestList = [{a, 3}, {b, 3}, {c, 3}, {d, a}, {e, a}, {f, a}],
    lists:foreach(fun({Key, Value})->merle:set(Key, Value) end, TestList).
    
test2()->
    ketama:start_link("ketama.servers"),
    lists:foreach(fun(Key)->
        Bin = <<Key>>,
        ketama:getserver(Bin) 
    end, lists:seq(1,1000)).