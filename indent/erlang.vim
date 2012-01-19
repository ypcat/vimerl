" Vim indent file
" Language: Erlang
" Author:   Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
" License:  Vim license
" Version:  2012/01/19

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

" TODO: writefile(), readfile() funcionan sobre un FIFO con os:cmd("cat fifo"), usar 2 FIFOs.
" TODO: optimizar y solo enviar desde la ultima linea que sea: ^\(-\)[a-z']
function ErlangIndent()
	if v:lnum == 1
		return 0
	else
		let code = join(getline(1, v:lnum), '\n')
		let indent = split(system(s:erlang_indent_file . ' ' . v:lnum, code))
		if len(indent) == 1
			return indent[0] * &shiftwidth
		else
			return indent[1]
		endif
	endif
endfunction
