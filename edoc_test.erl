-module(edoc_test).

-compile(export_all).

%%% -----------------------------------------------------------------------------
%%% http://userprimary.net/posts/2011/02/16/Generating-XML-in-Erlang-Using-xmerl/
%%% http://muharem.wordpress.com/2007/08/21/processing-xml-in-erlang/
%%% -----------------------------------------------------------------------------

run(ModName) ->
    Mod = list_to_atom(ModName),
    {File0, _} = filename:find_src(Mod),
    File = File0 ++ ".erl",
    {_, Doc} = edoc:get_doc(File),
    Xml = xmerl:export([Doc], xmerl_xml),
    io:format("Xml = ~p~n", [Xml]).
