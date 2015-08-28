#!/usr/bin/env escript

main([]) ->
    case file:consult("rebar.config") of
        {ok, Terms} ->
            ErlOpts = proplists:get_value(erl_opts, Terms),
            Includes = proplists:get_all_values(i, ErlOpts),
            Output = string:join(Includes, ","),
            io:format("~s", [Output]);
        {error, _} ->
            ok
    end.
