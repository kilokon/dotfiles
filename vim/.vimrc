" Basic configurations
if &compatible
  set nocompatible
endif

" disable plugins
let g:loaded_matchit           = 1
let g:loaded_logiPat           = 1
let g:loaded_rrhelper          = 1
let g:loaded_tarPlugin         = 1
let g:loaded_gzip              = 1
let g:loaded_zipPlugin         = 1
let g:loaded_2html_plugin      = 1
let g:loaded_shada_plugin      = 1
let g:loaded_spellfile_plugin  = 1
let g:loaded_netrw             = 1
let g:loaded_netrwPlugin       = 1
let g:loaded_tutor_mode_plugin = 1
let g:loaded_remote_plugins    = 1

" file related
set fileformat=unix
set fileformats=unix,dos,mac
set fileencoding=utf-8
filetype on
filetype plugin on
filetype indent on
syntax on

" indent
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2
set autoindent
set smartindent
augroup forbid_auto_comment_out
  autocmd!
  autocmd BufEnter * set formatoptions-=t
  autocmd BufEnter * set formatoptions-=c
  autocmd BufEnter * set formatoptions-=r
  autocmd BufEnter * set formatoptions-=o
augroup END

" Search
set incsearch
set ignorecase
set smartcase
set showcmd
set showmode
set showmatch
set hlsearch


set ttimeout		" time out for key codes
set ttimeoutlen=100	" wait up to 100ms after Esc for special key


" interface
set number
"set cursorline


set shiftwidth=4
set tabstop=4
set expandtab
set scrolloff=10

set nowrap

set wildmenu wildmode=list:longest
set wildignore+=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx,*.so,*~,*/.git/*,*/.svn/*,*/.DS_Store,*/tmp/*

" files/undo files/ temp files
set backupdir=~/.vim/backup/      " backup files (~) in a common location if possible
set backup
" set nobackup
set undodir=~/.vim/undo/
set undofile
set undolevels=1000 "maximum number of changes that can be undone
set undoreload=10000 "maximum number lines to save for undo on a buffer reload
set history=1000
set autoread
let mapleader = "\<Space>"
let g:mapleader = "\<Space>"

iab Ydate <C-R>=strftime("%Y-%m-%d")<CR>
" Example: 2002-04-06
"
iab Ytime <C-R>=strftime("%H:%M")<CR>
" Example: 14:28
"
iab YDT <C-R>=strftime("%Y-%m-%d %H:%M")<CR>
" Example: 2002-06-24 20:44

iab BDate <C-R>=strftime("meta-creation_date: %d.%m.%Y %H:%M")<CR>



set laststatus=2
if has('statusline')
    set statusline=\ %f%m%r\ u+%B\ [%{strlen(&ft)?&ft:'aucun'},%{strlen(&fenc)?&fenc:&enc},%{&fileformat},ts:%{&tabstop}]%=l.%l/%L,c.%c%V\ %P
endif




" PLUGINS ---------------------------------------------------------------- {{{

call plug#begin('~/.vim/plugged')
  Plug 'justinmk/vim-sneak'
  Plug 'tpope/vim-fugitive'
  Plug 'dense-analysis/ale'
  Plug 'Townk/vim-autoclose'
  Plug 'tpope/vim-surround'
  Plug 'tomasr/molokai'
  Plug 'tomtom/tcomment_vim'

  Plug 'airblade/vim-gitgutter'



  Plug 'preservim/nerdtree'
  Plug 'prabirshrestha/vim-lsp'
  Plug 'mattn/vim-lsp-settings'
  Plug 'prabirshrestha/asyncomplete.vim'
  Plug 'prabirshrestha/asyncomplete-lsp.vim'



call plug#end()


let g:sneak#label = 1
map f <Plug>Sneak_f
map F <Plug>Sneak_F
map t <Plug>Sneak_t
map T <Plug>Sneak_T




map <F2> :NERDTreeToggle<cr>
let NERDTreeIgnore=['.vim$', '\~$', '.*\.pyc$', '^\.svn$']




" Your vimrc
function! GitStatus()
  let [a,m,r] = GitGutterGetHunkSummary()
  return printf('+%d ~%d -%d', a, m, r)
endfunction
set statusline+=%{GitStatus()}
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)
nmap ghs <Plug>(GitGutterStageHunk)
nmap ghu <Plug>(GitGutterUndoHunk)
nmap ghp <Plug>(GitGutterPreviewHunk)



" In ~/.vim/vimrc, or somewhere similar.
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['eslint'],
\}
let g:ale_fix_on_save = 1


if has('gui')
    let g:molokai_original=1
    colorscheme molokai
    set guioptions-=m
    set guioptions-=T
else
    " Force 256color mode (for FreeBSD, bad termcap)
    if $TERM =~ "xterm-256color" || $TERM =~ "screen"
        set t_Co=256
    endif

    if &t_Co > 255
        let g:molokai_original=1
        let g:rehash256=1
        colorscheme molokai
        " Some additional conf not in molokai
    else
        colorscheme delek
    endif
endif


" }}}
