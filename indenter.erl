-module(indenter).

-compile(export_all).

-export([indent_file/2]).

%%% TODO TODO TODO
%%% indent_file(File)
%%% indent_file(File, Start, End)
%%%
%%% Hacer tambien rutinas para que muestre el fichero indentado
%%% TODO TODO TODO

indent_file(File, Line) ->
    % FIXME: hacer el try-catch en indent_file/1,2,3
    Tokens = tokenize_file(File),
    Tokens2 = indenter:take_tokens_block(Tokens, Line),
    indent_after(Tokens2).

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

eat_shebang([T1 = {'#', _}, T2 = {'!', _} | Tokens]) ->
    case {line(T1), line(T2)} of
        {N, N} ->
            lists:dropwhile(fun(T) -> line(T) == N end, Tokens);
        _ ->
            Tokens
    end;
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

indent_after(Tokens) ->
    try
        filter_no_column(parse_tokens(Tokens))
    catch
        throw:{parse_error, #state{tabs = Tabs, cols = Cols}} ->
            filter_no_column({hd(Tabs), hd(Cols)})
    end.

filter_no_column({Tab, none}) -> Tab;
filter_no_column({Tab, Col})  -> {Tab, Col}.

%% TODO: Puede no hacer match ninguna funcion de las de abajo
parse_tokens(Tokens = [{'-', _} | _]) ->
    parse_attribute(Tokens, #state{});
parse_tokens(Tokens = [{atom, _, _} | _]) ->
    parse_function(Tokens, #state{}).

parse_attribute([T = {'-', _} | Tokens], State = #state{stack = []}) ->
    parse_generic(Tokens, indent(State#state{stack = [T]}, 0)).

parse_function([T = {atom, _, _} | Tokens], State = #state{stack = []}) ->
    parse_generic(Tokens, indent(State#state{stack = [T]}, 2)).

indent(State, Tab) ->
    indent(State, Tab, none).

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

parse_generic2([T = {'(', _} | Tokens], State) ->
    parse_generic(Tokens, push(State, T, 0, column(T) + 1));
parse_generic2([{')', _} | Tokens], State = #state{stack = [{'(', _} | _]}) ->
    parse_generic(Tokens, pop(State));
parse_generic2([{dot, _}], #state{stack = [X]}) when element(1, X) == '-'; element(1, X) == atom ->
    {0, 0};
parse_generic2([], #state{tabs = [Tab | _], cols = [Col | _]}) ->
    {Tab, Col};
parse_generic2(_, State) ->
    throw({parse_error, State}).

next_relevant_token(Tokens) ->
    lists:dropwhile(fun irrelevant_token/1, Tokens).

irrelevant_token(Token) ->
    Chars = ['(', ')', '{', '}', '[', ']', '->', ',', ';', dot],
    Keywords = ['when', 'receive', 'fun', 'if', 'case', 'try', 'catch', 'after', 'end'],
    Cat = category(Token),
    not lists:member(Cat, Chars ++ Keywords).

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
