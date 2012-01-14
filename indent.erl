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
    LinesToks = lists:map(fun tokenize/1, Lines),
    case LinesToks of
        [L | Ls] ->
            [eat_shebang(L) | Ls];
        [] ->
            []
    end.

tokenize(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    Tokens.

eat_shebang([{'#', L}, {'!', L} | Tokens]) ->
    lists:dropwhile(fun(T) -> line(T) =< L end, Tokens);
eat_shebang(Tokens) -> Tokens.









% XXX: indent_file_line(Source, N)
indent_source_line(Source, N) when N < 1 ->
    error(badarg, [Source, N]);
indent_source_line(_, 1) ->
    0;
indent_source_line(Source, N) ->
    case tokenize_source(Source) of
        [] ->
            0;
        LinesToks ->
            try
                {PrevLine, RestLines} = search_prev_line(LinesToks, N),
                io:format
            catch
                line_not_found ->
                    0
            end
    end.

search_prev_line(LinesToks, N) ->
    search_prev_line2(lists:reverse(LinesToks), N).

search_prev_line2(LinesToks, N) when N < 1 ->
    error(badarg, [LinesToks, N]);
search_prev_line2([], _) ->
    throw(line_not_found);
search_prev_line2([Tokens | LinesToks], N) ->
    RevToks = lists:reverse(Tokens),
    case lists:dropwhile(fun(T) -> line(T) >= N end, RevToks) of
        [] ->
            search_prev_line2(LinesToks, N);
        RevToks2 ->
            {RevToks2, LinesToks}
    end.

line(Token) -> element(2, Token).

%%% ----------------------------------------------------------------------------
%%% Tests
%%% ----------------------------------------------------------------------------

-define(TEST_SRC, "%%% Comment\n #!/usr/bin/env escript\n -define(X, 6).\n foo(_) -> ?FOO. % Shit!\n").

tokenize_source_test() ->
    io:format("~p~n", [tokenize_source(?TEST_SRC)]).
