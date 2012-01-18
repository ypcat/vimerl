" Vim indent file
" Language: Erlang
" Author:   Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
" License:  Vim license
" Version:  2012/01/18

if exists('b:did_indent')
	finish
else
	let b:did_indent = 1
endif

setlocal indentexpr=ErlangIndent()
setlocal indentkeys=!^F,o,O,=after,=catch,=end,=),=},=]

if exists('*ErlangIndent')
	finish
endif

let s:erlang_indent_file = expand('<sfile>:p:h') . '/erlang_indent.erl'

function ErlangIndent()
	" TODO: Faster with erl -noshell -run script main ... -run erlang halt
	let indentation = split(system(s:erlang_indent_file . ' ' . expand('%') . ' ' . v:lnum))

	if len(indentation) == 1
		return indentation[0] * &shiftwidth
	else
		return indentation[1]
	endif
endfunction
