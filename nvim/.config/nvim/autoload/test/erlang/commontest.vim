if !exists('g:test#erlang#commontest#file_pattern')
  let g:test#erlang#commontest#file_pattern = '_SUITE\.erl$'
endif

function! test#erlang#commontest#test_file(file) abort
  return a:file =~# g:test#erlang#commontest#file_pattern
endfunction

function! test#erlang#commontest#build_position(type, position) abort
  if a:type == 'file'
    return ["--suite=" . fnamemodify(a:position['file'], ":r")]
  else
    " Placeholder, need to round up to nearest function name
    return ["--suite=" . fnamemodify(a:position['file'], ":r")]
  endif
endfunction

function! test#erlang#commontest#build_args(args) abort
  return a:args
endfunction

function! test#erlang#commontest#executable() abort
  return 'rebar3 ct'
endfunction
