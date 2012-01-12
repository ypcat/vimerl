-module(edoc_test).

-export([module_edoc/1]).

-include_lib("xmerl/include/xmerl.hrl").

module_edoc([ModName]) ->
    module_edoc(ModName);
module_edoc(ModName) ->
    Mod = list_to_atom(ModName),
    {File0, _} = filename:find_src(Mod),
    File = File0 ++ ".erl",
    {_, Doc} = edoc:get_doc(File),
    Funs = xmerl_xpath:string("/module/functions/function", Doc),
    FunsInfo = lists:map(fun inspect_function/1, Funs),
    FunsInfo2 = lists:keysort(1, FunsInfo),
    lists:foreach(
        fun({Name, Args, Return}) ->
                io:format("~s~n", [function_info_to_str(Name, Args, Return)])
        end,
        FunsInfo2).
%module_edoc(_) ->
    %bad_module.

inspect_function(Fun) ->
    Name = get_attribute(Fun, "name"),
    Args0 = xmerl_xpath:string("typespec/type/fun/argtypes/type", Fun),
    Args = lists:map(fun(Arg) -> get_attribute(Arg, "name") end, Args0),
    Return = inspect_function_return(Fun),
    {Name, Args, Return}.

inspect_function_return(Fun) ->
    [ReturnType] = xmerl_xpath:string("typespec/type/fun/type/*", Fun),
    simplify_return_type(xmerl_lib:simplify_element(ReturnType)).

simplify_return_type({type, _, [Type]}) ->
    simplify_return_type(Type);
simplify_return_type({tuple, _, Types}) ->
    Elems = lists:map(fun simplify_return_type/1, Types),
    "{" ++ string:join(Elems, ", ") ++ "}";
simplify_return_type({list, _, Types}) ->
    Elems = lists:map(fun simplify_return_type/1, Types),
    "[" ++ string:join(Elems, ", ") ++ "]";
simplify_return_type({typevar, [{name, Name}], _}) ->
    Name;
simplify_return_type({atom, [{value, Val}], _}) ->
    Val;
simplify_return_type({abstype, _, [Type]}) ->
    {erlangName, [{name, Name}], []} = Type,
    Name ++ "()";
simplify_return_type({union, _, Types}) ->
    Elems = lists:map(fun simplify_return_type/1, Types),
    string:join(Elems, " | ");
simplify_return_type(_) ->
    io:format("BIG SHIT SOMETIME HAPPENS!~n"),
    erlang:halt().

function_info_to_str(Name, Args, Return) ->
    io_lib:format("~s(~s) -> ~s", [Name, string:join(Args, ", "), Return]).

get_attribute(Elem, AttrName) ->
    [Attr] = xmerl_xpath:string("@" ++ AttrName, Elem),
    Attr#xmlAttribute.value.
