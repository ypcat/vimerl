#!/usr/bin/env escript

%-module(indenter).
-compile(export_all).
%-export([file_indentation/2]).

-define(TOKEN_IS(Token, Cat), element(1, Token) == Cat).

main([File, Line]) ->
    case file_indentation(File, list_to_integer(Line)) of
        {Tab, Col} ->
            io:format("~B ~B~n", [Tab, Col]);
        Tab ->
            io:format("~B~n", [Tab])
    end;
main(_) ->
    'XXX'.

%%% TODO TODO TODO
%%% indent_file(File)
%%% indent_file(File, Start, End)
%%%
%%% Hacer tambien rutinas para que muestre el fichero indentado
%%% TODO TODO TODO

% TODO: usar string:strip() para quitar los espacios de una linea y luego indentarla
% indent_file() ->

file_indentation(File, Line) ->
    % FIXME: hacer el try-catch en indent_file/1,2,3
    Tokens = tokenize_file(File),
    Tokens2 = take_tokens_block(Tokens, Line),
    % TODO: este retorna la indentacion, indent_file() lo indenta.
    indentation_after(Tokens2).

read_file(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            binary_to_list(Bin);
        Error ->
            throw(Error)
    end.

tokenize_file(File) ->
    try
        tokenize_source(read_file(File))
    catch
        throw:Error ->
            Error
    end.

tokenize_source(Source) ->
    try
        eat_shebang(tokenize(Source))
    catch
        throw:Error ->
            Error
    end.

tokenize(Source) ->
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

take_tokens_block(Tokens, Line) when Line < 1 ->
    error(badarg, [Tokens, Line]);
take_tokens_block(Tokens, Line) ->
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
        filter_no_column(parse_tokens(Tokens))
    catch
        throw:{parse_error, #state{tabs = Tabs, cols = Cols}} ->
            %io:format("parse_error~n"), % XXX
            filter_no_column({hd(Tabs), hd(Cols)})
    end.

filter_no_column({Tab, none}) -> Tab;
filter_no_column({Tab, Col})  -> {Tab, Col}.

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
    take_tokens_block(tokenize_source(?TEST_SOURCE), 14).
