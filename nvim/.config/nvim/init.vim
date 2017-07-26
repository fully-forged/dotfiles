" Plug {{{
call plug#begin('~/.config/nvim/bundle')

Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tmhedberg/matchit'
Plug 'vim-scripts/tComment'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'ervandew/supertab'
Plug 'ntpeters/vim-better-whitespace'
Plug 'mattn/emmet-vim'
Plug 'vim-ruby/vim-ruby'
Plug 'elixir-lang/vim-elixir'
Plug 'JulesWang/css.vim'
Plug 'kana/vim-textobj-user'
Plug 'nelstrom/vim-textobj-rubyblock', { 'for': 'ruby'}
Plug 'wolfy87/vim-enmasse'
Plug 'ElmCast/elm-vim'
Plug 'benmills/vimux'
Plug 'janko-m/vim-test'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'godlygeek/tabular'
Plug 'rking/ag.vim'
Plug 'slashmili/alchemist.vim', { 'for': 'elixir'}
Plug 'nonsense/tomorrow-night-vim-theme'
Plug 'vim-erlang/vim-erlang-runtime', { 'for': 'erlang'}
Plug 'vim-erlang/vim-erlang-omnicomplete', { 'for': 'erlang'}
Plug 'vim-erlang/vim-erlang-tags', { 'for': 'erlang'}
Plug 'vim-erlang/vim-erlang-skeletons', { 'for': 'erlang'}
Plug 'edkolev/erlang-motions.vim', { 'for': 'erlang'}
Plug 'dag/vim2hs', { 'for': 'haskell'}
Plug 'bitc/vim-hdevtools', { 'for': 'haskell'}
Plug 'w0rp/ale'
Plug 'mhinz/vim-startify'

call plug#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
" PlugInstall
" }}}
" {{{ Defaults
set nobackup
set nowritebackup
set notimeout
set ttimeout
set ttimeoutlen=10
set noswapfile
set history=50
set ruler
set showcmd
set incsearch
set laststatus=2
set t_Co=256
set modeline
set autoread
set tabstop=2
set shiftwidth=2
set expandtab
set foldmethod=indent
set foldlevelstart=99
syntax on
set number
" set numberwidth=2
" }}}
" {{{ Spellcheck
set spelllang=en
set spellfile=$HOME/.vim/spell/en.utf-8.add
" When spellcheck is enabled, autocomplete from dictionary
set complete+=kspell
" }}}
" Airline {{{
let g:rehash256 = 1

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = ''
let g:airline_right_sep = '«'
let g:airline_right_sep = ''
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

let g:airline#extensions#tabline#enabled = 1
call airline#parts#define_function('ALE', 'ALEGetStatusLine')
call airline#parts#define_condition('ALE', 'exists("*ALEGetStatusLine")')

let g:airline_section_error = airline#section#create_right(['ALE'])
let g:airline_theme='tomorrow'
"}}}
" {{{ Mouse, OS integration
" Fix backspace
set backspace=indent,eol,start

" Send more characters for redraws
set ttyfast

" Use Bash as shell
set shell=/usr/local/bin/bash

" Enable mouse use in all modes
set mouse=a

" Clipboard fix for OsX
set clipboard=unnamed
" }}}
" CTags {{{
" Tags
let Tlist_Ctags_Cmd = "/usr/local/bin/ctags -R --exclude=.git --exclude=log -f ./.tags *"
set tags+=.tags
" }}}
" Autocommands {{{
augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  au FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  au BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
  " Instead of reverting the cursor to the last position in the buffer, we
  " set it to the first line when editing a git commit message
  " Thanks: https://github.com/spf13/spf13-vim/blob/3.0/.vimrc#L92-L94
  au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])
  " Remove trailing whitespace before saving a file
  au FileType ruby,javascript,css,scss,sass,html autocmd BufWritePre <buffer> :%s/\s\+$//e
augroup END

" Save when losing focus
au FocusLost * :silent! wall

" Enable spellcheck when needed
autocmd FileType gitcommit setlocal spell textwidth=72
autocmd BufRead,BufNewFile {*.md,*.mkd} setlocal spell

" Soft wrap when writing docs
autocmd BufRead,BufNewFile {*.txt,*.tex,*.md} set wrap linebreak nolist textwidth=0 wrapmargin=0

" Run Ale only on save
let g:ale_lint_on_text_changed = 'never'
" Don't run ale when entering a file
let g:ale_lint_on_enter = 0
" }}}
" Shortcuts {{{

" Set leader key
let mapleader = ","

" Help for word under cursor
:map <leader>h "zyw:exe "h ".@z.""<CR>

" Make
nnoremap <leader>m :make<CR>

" Fixes pasting
noremap <leader>y "*y
noremap <leader>p :set paste<CR>"*p<CR>:set nopaste<CR>
noremap <leader>P :set paste<CR>"*P<CR>:set nopaste<CR>"

" Get off my lawn
nnoremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
nnoremap <Down> :echoe "Use j"<CR>

" map ctrl-hjkl for easy window movement
map <c-h> <c-w>h
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l

" Faster shortcut for commenting. Requires T-Comment plugin
map <leader>c <c-_><c-_>

"Saves time; maps the spacebar to colon
nmap <space> :

"Bubble single lines (kicks butt)
"http://vimcasts.org/episodes/bubbling-text/
nmap <C-Up> ddkP
nmap <C-Down> ddp
nmap <C-Left> <<
nmap <C-Right> >>

"Horizontal bubbling
vnoremap < <gv
vnoremap > >gv
nmap gV `[v`]

"Bubble multiple lines
vmap <C-Up> xkP`[V`]
vmap <C-Down> xp`[V`]
vmap <C-Right> >gv
vmap <C-Left> <gv

"tab navigation
nmap <silent> <Left> :bprevious<cr>
nmap <silent> <Right> :bnext<cr>

" Emmet
let g:user_emmet_expandabbr_key = '<c-e>'

" Remap NerdTree
nmap <silent> <leader>3 :NERDTreeToggle<cr>

" Remap CtrlP
nmap <silent> <leader>1 :CtrlP<cr>

autocmd filetype clojure nmap <leader>e :Eval<cr>

" Testing

nmap <silent> <leader>f :TestNearest<CR>
nmap <silent> <leader>t :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>

" Terminal
nmap <silent> <leader>h :split term://bash<CR>i
nmap <silent> <leader>v :vsplit term://bash<CR>i
tnoremap <Esc> <C-\><C-n>

" Location navigation
nmap <silent> <leader>n :lnext<cr>

"enable . in visual mode
vnoremap . :norm.<CR>

" Center banner around word
nnoremap <leader>b :center 80<cr>hhv0r#A<space><esc>40A#<esc>d80<bar>YppVr#kk.

" Start EnMasse
nmap <leader>e :EnMasse<CR>
" }}}
" Autocompletion {{{
set completeopt=longest,menuone
let g:SuperTabDefaultCompletionType = "context"
" }}}
" Syntax - General {{{
" Fake '|' as text object
nnoremap di\| T\|d,
nnoremap da\| F\|d,
nnoremap ci\| T\|c,
nnoremap ca\| F\|c,
nnoremap yi\| T\|y,
nnoremap ya\| F\|y,
nnoremap vi\| T\|v,
nnoremap va\| F\|v,

" Fake '/' as text object
nnoremap di/ T/d,
nnoremap da/ F/d,
nnoremap ci/ T/c,
nnoremap ca/ F/c,
nnoremap yi/ T/y,
nnoremap ya/ F/y,
nnoremap vi/ T/v,
nnoremap va/ F/v,

" }}}
" Syntax - Ruby {{{
" Fixes syntax highlight for Ruby files
au BufRead,BufNewFile {Capfile,Gemfile,Rakefile,Thorfile,Vagrantfile,config.ru,.caprc,.irbrc,irb_tempfile*} set ft=ruby
" }}}
" Syntax - Erlang/Elixir {{{
let g:alchemist#elixir_erlang_src = "~/oss"
" }}}
" Tmux {{{
"Key fixes for Tmux
map [A <C-Up>
map [B <C-Down>
map [D <C-Left>
map [C <C-Right>

" Vimux
let VimuxHeight = "30"
let VimuxOrientation = "v"
let VimuxUseNearestPane = 1

if exists('$TMUX')
  let g:test#strategy = 'vimux'

  " Prompt for a command to run
  map <Leader>rp :VimuxPromptCommand<CR>

  " Run last command executed by RunVimTmuxCommand
  map <Leader>rl :VimuxRunLastCommand<CR>

  " Inspect runner pane
  map <Leader>ri :VimuxInspectRunner<CR>

  " Interrupt any command running in the runner pane
  map <Leader>rs :VimuxInterruptRunner<CR>

  " If text is selected, save it in the v buffer and send that buffer it to tmux
  vmap <Leader>r "vy :call VimuxRunCommand(@v)

  " Select current paragraph and send it to tmux
  nmap <Leader>r vip<Leader>r
endif
"
" }}}
" CtrlP {{{
let g:ctrlp_working_path_mode = 2
set wildignore+=*/.hg/*,*/.svn/*,*/vendor/cache/*,*/public/system/*,*/tmp/*,*/log/*,*/.git/*,*/.jhw-cache/*,*/solr/data/*,*/node_modules/*,*/.DS_Store
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_extensions = ['tag']

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
" See discussion at: https://github.com/thoughtbot/dotfiles/commit/f854c8d8ef08ab0f80639e0219f9800f0246fb90
if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif


" }}}
" NERDTree {{{
let NERDTreeShowHidden=0
let NERDTreeShowBookmarks=0
let g:NERDTreeChDirMode=2
" }}}
" Vim-Test {{{
let test#filename_modifier = ':p'
let test#runners = {'erlang': ['CommonTest']}
" }}}
" {{{ Visual
" Change cursor for insert mode
set background=dark
colorscheme Tomorrow-Night

" Split right and below
set splitright
set splitbelow

" Don't redraw unnecessarily
set lazyredraw

" Display extra whitespace
set list listchars=tab:»·,trail:·

" Ale markers configuration
let g:ale_sign_error = '⨉'
let g:ale_sign_warning = '⚠'

" Ale status line
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']

" }}}
" vim:foldmethod=marker:foldlevel=0
