" Plug {{{
call plug#begin('~/.config/nvim/bundle')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tmhedberg/matchit'
Plug 'vim-scripts/tComment'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-projectionist'
Plug 'machakann/vim-highlightedyank'
Plug 'ervandew/supertab'
Plug 'ntpeters/vim-better-whitespace'
Plug 'mattn/emmet-vim'
Plug 'vim-ruby/vim-ruby'
Plug 'elixir-editors/vim-elixir'
Plug 'JulesWang/css.vim'
Plug 'kana/vim-textobj-user'
Plug 'nelstrom/vim-textobj-rubyblock', { 'for': 'ruby'}
Plug 'wolfy87/vim-enmasse'
Plug 'ElmCast/elm-vim', { 'for': 'elm'}
Plug 'benmills/vimux'
Plug 'janko-m/vim-test'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'godlygeek/tabular'
Plug 'mileszs/ack.vim'
Plug 'slashmili/alchemist.vim', { 'for': 'elixir'}
Plug 'nonsense/tomorrow-night-vim-theme'
Plug 'vim-erlang/vim-erlang-runtime', { 'for': 'erlang'}
Plug 'vim-erlang/vim-erlang-omnicomplete', { 'for': 'erlang'}
Plug 'vim-erlang/vim-erlang-tags', { 'for': 'erlang'}
Plug 'vim-erlang/vim-erlang-skeletons', { 'for': 'erlang'}
Plug 'edkolev/erlang-motions.vim', { 'for': 'erlang'}
Plug 'andyl/vim-projectionist-elixir', { 'for': 'elixir'}
Plug 'dag/vim2hs', { 'for': 'haskell'}
Plug 'alx741/vim-hindent', { 'for': 'haskell'}
Plug 'parsonsmatt/intero-neovim', { 'for': 'haskell'}
Plug 'mhinz/vim-startify'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'jlesquembre/base16-neovim'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'lambdalisue/gina.vim'
Plug 'neomake/neomake'
Plug 'mhinz/vim-mix-format', {'for': 'elixir'}
Plug 'tpope/vim-fireplace', {'for': 'clojure'}
Plug 'markwoodhall/vim-figwheel', {'for': 'clojure'}
Plug 'bhurlow/vim-parinfer', {'for': 'clojure'}
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Erichain/vim-monokai-pro'
Plug 'zyedidia/literate.vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'tpope/vim-dispatch'

call plug#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
" PlugInstall
" }}}
" {{{ Defaults
set cursorline
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
set inccommand=nosplit
set laststatus=2
set t_Co=256
set modeline
set autoread
set tabstop=2
set shiftwidth=2
set expandtab
set foldmethod=syntax
set foldlevelstart=99
set ignorecase
set smartcase
syntax on
set number

let g:python_host_prog = '/usr/local/bin/python2'
let g:python3_host_prog = '/usr/local/bin/python3'

set undodir=~/.nvim/undodir

set diffopt=filler,context:0
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
call airline#parts#define_function('GinaBranch', 'gina#component#repo#branch')
call airline#parts#define_condition('ALE', 'exists("*ALEGetStatusLine")')

let g:airline_section_error = airline#section#create_right(['ALE'])
let g:airline_section_b = airline#section#create_left(['GinaBranch'])
let g:airline_theme='base16_monokai'
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

call neomake#configure#automake('w')

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

" Format haskell
autocmd BufWritePost *.hs silent :!stack exec hfmt -- -w %

" Soft wrap when writing docs
autocmd BufRead,BufNewFile {*.txt,*.tex,*.md} set wrap linebreak nolist textwidth=0 wrapmargin=0

" Format Elixir code on save
let g:mix_format_on_save = 1

" Only display a warning when formatting fails due to compile errors
let g:mix_format_silent_errors = 1

" Use static formatter path
let g:mix_format_elixir_bin_path = '~/.kiex/elixirs/elixir-1.6.2/bin'

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

" Meta keys movement

" Terminal mode:
tnoremap <M-h> <c-\><c-n><c-w>h
tnoremap <M-j> <c-\><c-n><c-w>j
tnoremap <M-k> <c-\><c-n><c-w>k
tnoremap <M-l> <c-\><c-n><c-w>l
" Insert mode:
inoremap <M-h> <Esc><c-w>h
inoremap <M-j> <Esc><c-w>j
inoremap <M-k> <Esc><c-w>k
inoremap <M-l> <Esc><c-w>l
" Visual mode:
vnoremap <M-h> <Esc><c-w>h
vnoremap <M-j> <Esc><c-w>j
vnoremap <M-k> <Esc><c-w>k
vnoremap <M-l> <Esc><c-w>l
" Normal mode:
nnoremap <M-h> <c-w>h
nnoremap <M-j> <c-w>j
nnoremap <M-k> <c-w>k
nnoremap <M-l> <c-w>l

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
nmap <silent> <Left> :tabprevious<cr>
nmap <silent> <Right> :tabnext<cr>

" Emmet
let g:user_emmet_expandabbr_key = '<c-e>'

" Remap FZF
nmap <silent> <leader>1 :FZF<cr>

" Rotate splits

nmap <leader>vb :vert ball<CR>
nmap <leader>hb :ball<CR>

" Testing

nmap <silent> <leader>f :TestNearest<CR>
nmap <silent> <leader>t :TestFile<CR>
nmap <silent> <leader>l :TestLast<CR>

" Projections

nmap <silent> <leader>a :A<CR>

" Terminal
nmap <silent> <leader>h :botright 30split  term://fish<CR>i
nmap <silent> <leader>v :botright 120vsplit term://fish<CR>i
tnoremap <Esc> <C-\><C-n>

nmap <silent> <leader>3 :NERDTreeToggle<cr>

" Switch buffers via FZF
nmap <silent> <leader>s :Buffers<CR>

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
let g:deoplete#enable_at_startup = 1
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

au BufRead,BufNewFile {.envrc} set ft=sh

" }}}
" Syntax - Ruby {{{
" Fixes syntax highlight for Ruby files
au BufRead,BufNewFile {Capfile,Gemfile,Rakefile,Thorfile,Vagrantfile,config.ru,.caprc,.irbrc,irb_tempfile*} set ft=ruby
" }}}
" Syntax - Erlang/Elixir {{{
" let g:alchemist#elixir_erlang_src = "~/oss"
" }}}
" Syntax - Rust {{{
let g:rustfmt_autosave = 1
" }}}
" Syntax - Haskell {{{

let g:intero_type_on_hover = 1

augroup interoMaps
  au!
  " Maps for intero. Restrict to Haskell buffers so the bindings don't collide.

  " Background process and window management
  au FileType haskell nnoremap <silent> <leader>is :InteroStart<CR>
  au FileType haskell nnoremap <silent> <leader>ik :InteroKill<CR>

  " Open intero/GHCi split horizontally
  au FileType haskell nnoremap <silent> <leader>io :InteroOpen<CR>
  " Open intero/GHCi split vertically
  au FileType haskell nnoremap <silent> <leader>iov :InteroOpen<CR><C-W>H
  au FileType haskell nnoremap <silent> <leader>ih :InteroHide<CR>

  " Reloading (pick one)
  " Automatically reload on save
  au BufWritePost *.hs InteroReload
  " Manually save and reload
  " au FileType haskell nnoremap <silent> <leader>wr :w \| :InteroReload<CR>

  " Load individual modules
  au FileType haskell nnoremap <silent> <leader>il :InteroLoadCurrentModule<CR>
  au FileType haskell nnoremap <silent> <leader>if :InteroLoadCurrentFile<CR>

  " Type-related information
  " Heads up! These next two differ from the rest.
  au FileType haskell map <silent> <leader>t <Plug>InteroGenericType
  au FileType haskell map <silent> <leader>T <Plug>InteroType
  au FileType haskell nnoremap <silent> <leader>it :InteroTypeInsert<CR>

  " Navigation
  au FileType haskell nnoremap <silent> <leader>jd :InteroGoToDef<CR>

  " Managing targets
  " Prompts you to enter targets (no silent):
  au FileType haskell nnoremap <leader>ist :InteroSetTargets<SPACE>
augroup END
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

let g:asyncrun_open = 10
let g:test#strategy = 'neovim'

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
" FZF {{{
set wildignore+=*/.hg/*,*/.svn/*,*/vendor/cache/*,*/public/system/*,*/tmp/*,*/log/*,*/.git/*,*/.jhw-cache/*,*/solr/data/*,*/node_modules/*,*/.DS_Store

" Use Ripgrep for search
if executable('rg')
  " Use Ack with rg
  let g:ackprg = 'rg --vimgrep --ignore-case'

  " Use Ag over Grep
  set grepprg=rg\ --vimgrep\ --ignore-case
endif
" }}}
" File explorer {{{
let NERDTreeShowHidden=0
let NERDTreeShowBookmarks=0
let g:NERDTreeChDirMode=2
" }}}
" {{{ Visual
if (has("termguicolors"))
  let base16colorspace=256
  set termguicolors
endif

" Change cursor for insert mode
set background=dark
colorscheme base16-gruvbox-dark-hard

" Split right and below
set splitright
set splitbelow

" Don't redraw unnecessarily
set lazyredraw

" Display extra whitespace
set list listchars=tab:»·,trail:·

" NeoVim cursor
highlight! link TermCursor Cursor
highlight! TermCursorNC guibg=cc6666 guifg=white ctermbg=1 ctermfg=15
" }}}
" {{{ Startify
let g:startify_change_to_dir = 0
let g:startify_list_order = ['dir', 'sessions']

" }}}
" vim:foldmethod=marker:foldlevel=0
