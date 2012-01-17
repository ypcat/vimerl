" Vim indent file
" Language: Erlang
" Author:   Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
" License:  Vim license
" Version:  2012/01/17

if exists("b:did_indent")
    finish
else
    let b:did_indent = 1
endif

setlocal indentexpr=ErlangIndent()
setlocal indentkeys+==after,=end,=catch,=),=],=} " XXX: Revisar

if exists("*ErlangIndent")
    finish
endif

function ErlangIndent()
    " v:lnum
    " getline(v:lnum)
    " indent(v:lnum)

    return 2 * &shiftwidth
endfunction
