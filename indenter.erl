-module(indenter).

-compile(export_all).

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

take_tokens_block(Tokens, N) when N < 1 ->
    error(badarg, [Tokens, N]);
take_tokens_block(Tokens, N) ->
    PrevToks = lists:reverse(lists:takewhile(fun(T) -> line(T) < N end, Tokens)),
    lists:reverse(lists:takewhile(fun(T) -> category(T) /= dot end, PrevToks)).

category(Token) -> erl_scan:token_info(Token, category).

line(Token) -> erl_scan:token_info(Token, line).

column(Token) -> erl_scan:token_info(Token, column).

%%% TODO -----------------------------------------------------------------------

-record(state, {stack = [], tabs = [0], cols = [0]}).
-record(indentation, {tab = 0, col = 0}).

parse_tokens(Tokens) ->
    try
        parse_tokens2(Tokens)
    catch
        throw:{parse_error, #state{tabs = Tabs, cols = Cols}} ->
            #indentation{tab = hd(Tabs), col = hd(Cols)}
    end.

parse_tokens2(Tokens = [{'-', _} | _]) ->
    parse_attribute(Tokens, #state{});
parse_tokens2(Tokens = [{atom, _, _} | _]) ->
    parse_function(Tokens, #state{}).

parse_attribute([T = {'-', _} | Tokens], State = #state{stack = [], tabs = []}) ->
    parse_generic(Tokens, State#state{stack = [T], tabs = [1]}).

parse_function([T = {atom, _, _} | Tokens], State = #state{stack = [], tabs = []}) ->
    parse_generic(Tokens, State#state{stack = [T], tabs = [2]}).

parse_generic(Tokens, State) ->
    parse_generic2(next_relevant_token(Tokens), State).



%%%%%%%%%%%%%%
% XXX: hay que indicar una columna siempre, por defecto la misma que el tab
%parse_generic2([T = {'(', _} | Tokens], State = #state{stack = Stack, tabs = Tabs, cols = Cols}) ->
%    Tab = hd(Tabs) + 2,
%    Col = 'XXX', % Coger aqui la columna + 1 del anterior (,{,[ del stack
%    parse_generic(Tokens, State#state{stack = [T | Stack], tab = [hd(Tabs) | Tabs]}); % XXX: Refinar
%parse_generic2([{')', _} | Tokens], State = #state{stack = [{'(', _} | Stack]}) ->
%%%%%%%%%%%%%%



parse_generic2([{dot, _}], _) ->
    #indentation{};
parse_generic2([], #state{tabs = [Tab | _], cols = [Col | _]}) ->
    #indentation{tab = Tab, col = Col};
parse_generic2(_, State) ->
    throw({parse_error, State}).

next_relevant_token(Tokens) ->
    lists:dropwhile(fun irrelevant_token/1, Tokens).

irrelevant_token(Token) ->
    Chars = ['(', '{', '[', '-', dot],
    KeyWords = ['receive', 'fun', 'if', 'case', 'try', 'catch', 'after', 'end'],
    Type = category(Token),
    not lists:keymember(Type, 1, Chars ++ KeyWords).

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
    io:format("~p~n", [tokenize_source(?TEST_SOURCE)]).

take_tokens_block_test() ->
    io:format("~p~n", [take_tokens_block(tokenize_source(?TEST_SOURCE), 14)]).
