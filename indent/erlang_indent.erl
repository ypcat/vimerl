#!/usr/bin/env escript

-define(TOKEN_IS(T, C), (element(1, T) == C)).
-define(TOKEN_OPEN(T), ?TOKEN_IS(T, '('); ?TOKEN_IS(T, '{'); ?TOKEN_IS(T, '[')).
-define(TOKEN_CLOSE(T), ?TOKEN_IS(T, ')'); ?TOKEN_IS(T, '}'); ?TOKEN_IS(T, ']')).
-define(TOKEN_NOT_OPEN(T), not ?TOKEN_IS(T, '('), not ?TOKEN_IS(T, '{'), not ?TOKEN_IS(T, '[')).

-record(state, {stack = [], tabs = [0], cols = [none]}).

main([Line, File]) ->
    Source = read_file(File),
    format_indentation(source_indentation(Source, list_to_integer(Line)));
main([Line]) ->
    Source = read_stdin(),
    format_indentation(source_indentation(Source, list_to_integer(Line)));
main(_) ->
    bad_opts.

read_file(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin).

read_stdin() ->
    read_stdin([]).

read_stdin(L) ->
    case io:get_chars("", 4096) of
        eof ->
            lists:flatten(lists:reverse(L));
        Data ->
            read_stdin([Data | L])
    end.

format_indentation({Tab, none}) ->
    io:format("~B~n", [Tab]);
format_indentation({Tab, Col}) ->
    io:format("~B ~B~n", [Tab, Col]).

source_indentation(Source, Line) ->
    Tokens = tokenize_source(Source),
    {PrevToks, NextToks} = split_prev_block(Tokens, Line),
    indentation_between(PrevToks, NextToks).

tokenize_source(Source) ->
    eat_shebang(tokenize_source2(Source)).

tokenize_source2(Source) ->
    case erl_scan:string(Source, {1, 1}) of
        {ok, Tokens, _} ->
            Tokens;
        {error, _, _} ->
            []
    end.

eat_shebang([{'#', {N, _}}, {'!', {N, _}} | Tokens]) ->
    lists:dropwhile(fun(T) -> line(T) == N end, Tokens);
eat_shebang(Tokens) ->
    Tokens.

split_prev_block(Tokens, Line) when Line < 1 ->
    error(badarg, [Tokens, Line]);
split_prev_block(Tokens, Line) ->
    {PrevToks, NextToks} = lists:splitwith(fun(T) -> line(T) < Line end, Tokens),
    PrevToks2 = lists:reverse(PrevToks),
    PrevToks3 = lists:takewhile(fun(T) -> category(T) /= dot end, PrevToks2),
    {lists:reverse(PrevToks3), NextToks}.

category(Token) ->
    {category, Cat} = erl_scan:token_info(Token, category),
    Cat.

line(Token) ->
    {line, Line} = erl_scan:token_info(Token, line),
    Line.

column(Token) ->
    {column, Col} = erl_scan:token_info(Token, column),
    Col.

indentation_between([], _) ->
    {0, none};
indentation_between(PrevToks, NextToks) ->
    try
        {Tab, Col} = parse_tokens(PrevToks),
        case NextToks of
            _ when Col == none ->
                {Tab, Col};
            [T | _] when ?TOKEN_CLOSE(T) ->
                {Tab, Col - 1};
            _ ->
                {Tab, Col}
        end
    catch
        throw:{parse_error, #state{tabs = Tabs, cols = Cols}} ->
            io:format("Error: parse_error thrown~n"), % XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX: Remove
            {hd(Tabs), hd(Cols)}
    end.

parse_tokens(Tokens = [{'-', _} | _]) ->
    parse_attribute(Tokens, #state{});
parse_tokens(Tokens = [{atom, _, _} | _]) ->
    parse_function(Tokens, #state{});
parse_tokens(_) ->
    throw({parse_error, #state{}}).

parse_attribute([T = {'-', _} | Tokens], State = #state{stack = []}) ->
    parse_generic(Tokens, push(State, T, 0));
parse_attribute(_, State) ->
    throw({parse_error, State}).

parse_function([T = {atom, _, _} | Tokens], State = #state{stack = []}) ->
    parse_generic(Tokens, push(State, T, 2));
parse_function(_, State) ->
    throw({parse_error, State}).

parse_generic(Tokens, State) ->
    parse_generic2(next_relevant_token(Tokens), State).

parse_generic2([T | Tokens], State) when ?TOKEN_OPEN(T) ->
    parse_generic(Tokens, push(State, T, 0, column(T)));
parse_generic2([T1 | Tokens], State = #state{stack = [T2 | _]}) when ?TOKEN_CLOSE(T1) ->
    case symmetrical(T1) == category(T2) of
        true ->
            parse_generic(Tokens, pop(State));
        false ->
            throw({parse_error, State})
    end;
parse_generic2([T1 = {'->', _} | Tokens], State = #state{stack = [T2]}) when ?TOKEN_IS(T2, atom) ->
    parse_generic(Tokens, push(pop(State), T1, 1));
parse_generic2([{';', _} | Tokens], State = #state{stack = [T | _]}) when ?TOKEN_NOT_OPEN(T) ->
    parse_generic(Tokens, pop(State));
parse_generic2([{dot, _} | Tokens], State = #state{stack = [T]}) when ?TOKEN_IS(T, '-'); ?TOKEN_IS(T, '->') ->
    parse_generic(Tokens, pop(State));
parse_generic2([], #state{tabs = [Tab | _], cols = [Col | _]}) ->
    {Tab, Col};
parse_generic2(_, State) ->
    throw({parse_error, State}).

indent(State, OffTab, Col) ->
    Tabs = State#state.tabs,
    Cols = State#state.cols,
    State#state{tabs = [hd(Tabs) + OffTab | Tabs], cols = [Col | Cols]}.

unindent(State = #state{tabs = Tabs, cols = Cols}) ->
    State#state{tabs = tl(Tabs), cols = tl(Cols)}.

push(State, Token, OffTab) ->
    push(State, Token, OffTab, none).

push(State = #state{stack = Stack}, Token, OffTab, Col) ->
    indent(State#state{stack = [Token | Stack]}, OffTab, Col).

pop(State = #state{stack = Stack}) ->
    unindent(State#state{stack = tl(Stack)}).

next_relevant_token(Tokens) ->
    lists:dropwhile(fun(T) -> irrelevant_token(T) end, Tokens).

irrelevant_token(Token) ->
    Chars = ['(', ')', '{', '}', '[', ']', '->', ';', dot],
    Keywords = ['when', 'receive', 'fun', 'if', 'case', 'try', 'catch', 'after', 'end'],
    Cat = category(Token),
    not lists:member(Cat, Chars ++ Keywords).

symmetrical(Token) when is_tuple(Token) ->
    symmetrical(category(Token));
symmetrical(')') -> '(';
symmetrical('}') -> '{';
symmetrical(']') -> '['.
