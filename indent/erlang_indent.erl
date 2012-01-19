#!/usr/bin/env escript

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(erlang_indent).
%-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TOKEN_IS(Token, Cat), element(1, Token) == Cat).

%%% TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
%%% TODO: hacer una rutina que retorne el estado del parser para indentar varias lineas
%%%
%%% indent_file(File)
%%% indent_file(File, Start, End)
%%%
%%% Hacer tambien rutinas para que muestre el fichero indentado
%%%
%%% TODO: usar string:strip() para quitar los espacios de una linea y luego indentarla
%%% TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO

%% TODO: Handle thrown exceptions
main([Line, File]) ->
    Source = read_file(File),
    format_indentation(source_indentation(Source, list_to_integer(Line)));
main([Line]) ->
    Source = read_stdin(),
    format_indentation(source_indentation(Source, list_to_integer(Line)));
main(_) ->
    bad_opts.

read_file(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            binary_to_list(Bin);
        Error ->
            throw(Error)
    end.

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
    Tokens2 = take_tokens_block_before(Tokens, Line),
    indentation_after(Tokens2).

tokenize_source(Source) ->
    eat_shebang(tokenize_source2(Source)).

tokenize_source2(Source) ->
    case erl_scan:string(Source, {1, 1}) of
        {ok, Tokens, _} ->
            Tokens;
        Error ->
            throw(Error)
    end.

eat_shebang([{'#', {N, _}}, {'!', {N, _}} | Tokens]) ->
    lists:dropwhile(fun(T) -> line(T) == N end, Tokens);
eat_shebang(Tokens) ->
    Tokens.

take_tokens_block_before(Tokens, Line) when Line < 1 ->
    error(badarg, [Tokens, Line]);
take_tokens_block_before(Tokens, Line) ->
    PrevToks = lists:reverse(lists:takewhile(fun(T) -> line(T) < Line end, Tokens)),
    lists:reverse(lists:takewhile(fun(T) -> category(T) /= dot end, PrevToks)).

category(Token) ->
    {category, Cat} = erl_scan:token_info(Token, category),
    Cat.

line(Token) ->
    {line, Line} = erl_scan:token_info(Token, line),
    Line.

column(Token) ->
    {column, Col} = erl_scan:token_info(Token, column),
    Col.

%%% TODO -----------------------------------------------------------------------

-record(state, {stack = [], tabs = [0], cols = [none]}).

indentation_after(Tokens) ->
    try
        parse_tokens(Tokens)
    catch
        throw:{parse_error, #state{tabs = Tabs, cols = Cols}} ->
            io:format("Parse error~n"), % XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX
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

indent(State, Tab, Col) ->
    Tabs = State#state.tabs,
    Cols = State#state.cols,
    State#state{tabs = [Tab + hd(Tabs) | Tabs], cols = [Col | Cols]}.

unindent(State = #state{tabs = Tabs, cols = Cols}) ->
    State#state{tabs = tl(Tabs), cols = tl(Cols)}.

push(State, Token, Tab) ->
    push(State, Token, Tab, none).

push(State = #state{stack = Stack}, Token, Tab, Col) ->
    indent(State#state{stack = [Token | Stack]}, Tab, Col).

pop(State = #state{stack = Stack}) ->
    unindent(State#state{stack = tl(Stack)}).

parse_generic(Tokens, State) ->
    parse_generic2(next_relevant_token(Tokens), State).

parse_generic2([T | Tokens], State) when ?TOKEN_IS(T, '('); ?TOKEN_IS(T, '{'); ?TOKEN_IS(T, '[') ->
    parse_generic(Tokens, push(State, T, 0, column(T) + 1));
parse_generic2([T1 | Tokens], State = #state{stack = [T2 | _]}) when ?TOKEN_IS(T1, ')'); ?TOKEN_IS(T1, '}'); ?TOKEN_IS(T1, ']') ->
    case symmetrical(T1) == category(T2) of
        true ->
            parse_generic(Tokens, pop(State));
        false ->
            throw({parse_error, State})
    end;
parse_generic2([{dot, _}], #state{stack = [T]}) when ?TOKEN_IS(T, '-'); ?TOKEN_IS(T, atom) ->
    {0, 0};
parse_generic2([], #state{tabs = [Tab | _], cols = [Col | _]}) ->
    {Tab, Col};
parse_generic2(_, State) ->
    throw({parse_error, State}).

next_relevant_token(Tokens) ->
    lists:dropwhile(fun(T) -> irrelevant_token(T) end, Tokens).
    %lists:dropwhile(fun irrelevant_token/1, Tokens). % XXX: not in escript

irrelevant_token(Token) ->
    Chars = ['(', ')', '{', '}', '[', ']', '->', ',', ';', dot],
    Keywords = ['when', 'receive', 'fun', 'if', 'case', 'try', 'catch', 'after', 'end'],
    Cat = category(Token),
    not lists:member(Cat, Chars ++ Keywords).

symmetrical(')')   -> '(';
symmetrical('}')   -> '{';
symmetrical(']')   -> '[';
symmetrical(Token) -> symmetrical(category(Token)).

%%% ----------------------------------------------------------------------------
%%% Tests
%%% ----------------------------------------------------------------------------

-define(TEST_SOURCE, "
#!/usr/bin/env escript

%%%
%%% Comment
%%%

-define(FOO, 666).

foo(N) -> % Shit!
    ?FOO + 1.

bar() ->
    Y = 123, % Line 14
    {ok, Y}.
").

tokenize_source_test() ->
    tokenize_source(?TEST_SOURCE).

take_tokens_block_test() ->
    take_tokens_block_before(tokenize_source(?TEST_SOURCE), 14).
