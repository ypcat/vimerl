-module(test).

-export([foo/0,
         baaaaar/2,
         baaaaaaaaa/4
        ]).

-export([
            foo/1,
            baaaaar/2,
            baaaaaaaaa/4]).

-spec foo(X) -> {ok, Key} | {false, X} when
    X :: iter(),
    Key :: integer().

foo() ->
    bar(1,
        2,
        3
       ),
    X = 1 + 2 +
    3 + 4, % FIXME
    ok.

foo() ->
    fuuuuuuuuuuuuur(
        1,
        2
        ),
    ok.

fooooooooo(X,
           Y) when
        X =:= Y ->
    ok.

foo() ->
    try
        foo()
    catch
        foo when
                foo ->
            bar()
    after
        bar()
    end.

foo() ->
    try
        foo(),
        bar()
    of
        foo when
                bar == 2 ->
            foo(),
            bar();
        bar ->
            foo
    end.

foo() ->
    try
        foo(),
        bar()
    of
        foo when
                bar == 2 ->
            foo(),
            bar();
        bar ->
            foo
    after
        foo(),
        bar()
    end.

foo() ->
    try
        foo(),
        bar()
    of
        foo when
                bar == 2 ->
            foo(),
            bar();
        bar ->
            foo
    after
        foo(),
        bar()
    end.

foo() ->
    try
        foo(),
        bar()
    of
        foo when
                bar == 2 ->
            foo(),
            bar();
        bar ->
            foo
    catch
        foo when
                foo ->
            foo
    after
        foo(),
        bar()
    end.

foo() ->
    receive
        foo when
                foo ->
            foo()
    end.

foo() ->
    receive
        foo when
                foo ->
            foo()
    after
        1000 ->
            bar()
    end.

foo() ->
    if
        foo ->
            bar();
        bar ->
            foo()
    end.

foo() ->
    case foo() of
        foo when
                bar ->
            foo();
        bar ->
    end.

foo() ->
    case
        foo()
    of
        foo when
                bar ->
            foo();
        bar ->
    end.
