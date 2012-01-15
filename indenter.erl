-module(indenter).

-compile(export_all).

read_file(File) ->
    % TODO: Leer por lineas con string:tokens(List, "\n")
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin).

tokenize_file(File) ->
    tokenize_source(read_file(File)).

tokenize_source(Source) ->
    eat_shebang(tokenize(Source)).

tokenize(Source) ->
    % TODO: Añadir a los tokens "({[]})" la columna.
    %       Ir buscando por linea la columna de cada uno.
    {ok, Tokens, _} = erl_scan:string(Source),
    Tokens.

eat_shebang([{'#', N}, {'!', N} | Tokens]) ->
    lists:dropwhile(fun(T) -> line(T) == N end, Tokens);
eat_shebang(Tokens) -> Tokens.

take_tokens_block(Tokens, N) when N < 1 ->
    error(badarg, [Tokens, N]);
take_tokens_block(Tokens, N) ->
    PrevToks = lists:reverse(lists:takewhile(fun(T) -> line(T) < N end, Tokens)),
    lists:reverse(lists:takewhile(fun(T) -> type(T) /= dot end, PrevToks)).

type(Token) -> element(1, Token).

line(Token) -> element(2, Token).

%%% --- TODO -------------------------------------------------------------------





% TODO: si falla el parseo, dar por defecto a la linea la misma
%       indentacion que tenia la anterior

% TODO: Proteger con un try si hay un badmatch
parse_block([{'-', _} | Tokens]) ->
    parse_attribute(Tokens);
% TODO: guardar tambien al funcion en el Stack esperando encontrar `->', mientras,
% guardar la columna igual a 2 tab
parse_block([{atom, _, _} | Tokens]) ->
    parse_function(Tokens, []).


parse_attribute1([{atom, _, _} | Tokens]) ->
    parse_attribute2(Tokens, []).

% TODO: Guardar la linea de un parenteses para luego poder encontrarlo
parse_attribute2([{'(', N} | Tokens], Stack) ->
    parse_attribute2(Tokens, [{'(', N}, Stack]);
parse_attribute2([{')', _} | Tokens], [_ | Stack]) ->
    parse_attribute2(Tokens, Stack);

parse_attribute2([{'.', _} | Tokens], [_ | Stack]) ->
    parse_attribute2(Tokens, Stack);

parse_attribute2([_ | _], Stack) ->
    % TODO: Usar la cola para indentar!
parse_attribute2([_ | _], []) -> 0.

%%% TODO TODO TODO
%%% En el Stack guardar solo los symbolos sin emparejar, una vez se entrentra el
%%% simetrico, sacarlo. Pero guardar tambien loa if, case y try.
%%%
%%% Hacer mejor un record para guardar el estado.
%%%
%%% Guardar en el estado el nivel de sangrado (tab) y si hay algo en el Stack,
%%% tambien la columna.
%%% TODO TODO TODO



parse_attribute([], []) -> 0;
parse_attribute([], Stack) ->



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
