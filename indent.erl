-module(indent).

-compile(export_all).

read_file_lines(File) ->
    {ok, Bin} = file:read_file(File),
    string:tokens(binary_to_list(Bin), "\n").

tokenize_source(Source) ->
    tokenize_lines([Source]).

tokenize_file(File) ->
    tokenize_lines(read_file_lines(File)).

tokenize_lines(Lines) ->
    LinesTok = lists:map(fun tokenize/1, Lines),
    case LinesTok of
        [L | Ls] ->
            [eat_shebang(L) | Ls];
        [] ->
            []
    end.

tokenize(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    Tokens.

eat_shebang([{'#', L}, {'!', L} | Tokens]) ->
    lists:dropwhile(fun(T) -> element(2, T) =< L end, Tokens);
eat_shebang(Tokens) -> Tokens.

%%% ----------------------------------------------------------------------------
%%% Tests
%%% ----------------------------------------------------------------------------

-define(TEST_SRC, "%%% Comment\n #!/usr/bin/env escript\n -define(X, 6).\n foo(_) -> ?FOO. % Shit!\n").

tokenize_source_test() ->
    io:format("~p~n", [tokenize_source(?TEST_SRC)]).
