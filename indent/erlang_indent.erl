#!/usr/bin/env escript

%%% ----------------------------------------------------------------------------
%%% TODO: Handle `fun'
%%% TODO: Handle split expression without the `,', use indent_next_token()?
%%% TODO: Handle -spec
%%% TODO: Handle `after' in the receive and the try
%%% ----------------------------------------------------------------------------

-record(state, {stack = [], tabs = [0], cols = [none]}).

-define(IS(T, C), (element(1, T) == C)).
-define(OPEN_BRACKET(T), ?IS(T, '('); ?IS(T, '{'); ?IS(T, '[')).
-define(CLOSE_BRACKET(T), ?IS(T, ')'); ?IS(T, '}'); ?IS(T, ']')).
-define(BRANCH_EXPR(T), ?IS(T, 'receive'); ?IS(T, 'if'); ?IS(T, 'case'); ?IS(T, 'try')).
-define(END_BRANCH_EXPR(T), ?IS(T, 'catch'); ?IS(T, 'after'); ?IS(T, 'end')). % FIXME: meter aqui el 'end' con los demas?

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

%% TODO: A lo mejor hay que retornar el State y que esta
%%       funcion lo trate.
indentation_between([], _) ->
    {0, none};
indentation_between(PrevToks, NextToks) ->
    try
        {Tab, Col} = parse_tokens(PrevToks),
        case NextToks of
            [T | _] when ?CLOSE_BRACKET(T) ->
                case Col of
                    none ->
                        {Tab, Col};
                    _ ->
                        {Tab, Col - 1}
                end;
            [T | _] when ?IS(T, 'of') ->
                {Tab - 1, none};
            % FIXME: el 'end' no lleva la misma indentacion que el resto
            [T | _] when ?END_BRANCH_EXPR(T) ->
                {Tab - 2, none};
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
    parse_next(Tokens, push(State, T, 0));
parse_attribute(_, State) ->
    throw({parse_error, State}).

parse_function([T = {atom, _, _} | Tokens], State = #state{stack = []}) ->
    parse_next(Tokens, push(State, T, 2));
parse_function(_, State) ->
    throw({parse_error, State}).

parse_next(Tokens, State) ->
    parse_next2(next_relevant_token(Tokens), State).

parse_next2([T | Tokens], State) when ?OPEN_BRACKET(T) ->
    parse_next(Tokens, push(State, T, 0, column(T)));
parse_next2([T1 | Tokens], State = #state{stack = [T2 | _]}) when ?CLOSE_BRACKET(T1) ->
    case symmetrical(T1) == category(T2) of
        true ->
            parse_next(Tokens, pop(State));
        false ->
            throw({parse_error, State})
    end;
parse_next2([T1 = {'->', _} | Tokens], State = #state{stack = [T2]}) when ?IS(T2, atom) ->
    parse_next(Tokens, push(State, T1, -1));
parse_next2([T1 = {'->', _} | Tokens], State = #state{stack = [T2 | _]}) when ?BRANCH_EXPR(T2) ->
    parse_next(Tokens, push(State, T1, 1));
parse_next2([T | Tokens], State) when ?BRANCH_EXPR(T) ->
    parse_next(Tokens, push(State, T, 1));
parse_next2([{';', _} | Tokens], State = #state{stack = [T1, T2 | _]}) when ?IS(T1, '->'), ?IS(T2, atom) ->
    parse_function(Tokens, pop(pop(State)));
parse_next2([{';', _} | Tokens], State = #state{stack = [T1, T2 | _]}) when ?IS(T1, '->'), ?BRANCH_EXPR(T2) ->
    parse_next(Tokens, pop(State));
parse_next2([{';', _} | Tokens], State) ->
    parse_next(Tokens, State);

%% FIXME: El 'end' de abajo es el que hace pop(pop(...))
parse_next2([T | Tokens], State = #state{stack = [{'->', _} | _]}) when ?END_BRANCH_EXPR(T) ->
    parse_next(Tokens, pop(State));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_next2([{'end', _} | Tokens], State = #state{stack = [T | _]}) when ?IS(T, '->') ->
    parse_next(Tokens, pop(pop(State)));
parse_next2([{dot, _} | Tokens], State = #state{stack = [T]}) when ?IS(T, '-'); ?IS(T, '->') ->
    parse_next(Tokens, pop(pop(State)));
parse_next2([], #state{tabs = [Tab | _], cols = [Col | _]}) ->
    {Tab, Col};
parse_next2(_, State) ->
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
    Keywords = ['receive', 'fun', 'if', 'case', 'try', 'catch', 'after', 'end'],
    Cat = category(Token),
    not lists:member(Cat, Chars ++ Keywords).

symmetrical(Token) when is_tuple(Token) ->
    symmetrical(category(Token));
symmetrical(')') -> '(';
symmetrical('}') -> '{';
symmetrical(']') -> '['.
