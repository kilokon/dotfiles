""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Make windows use ~/.vim too, I don't want to use _vimfiles
if has('win32') || has('win64')
    set runtimepath^=~/.vim
endif

set ttimeout		" time out for key codes
set ttimeoutlen=100	" wait up to 100ms after Esc for special key


set nocompatible            " Necessary for cool features of vim

syntax enable               " Syntax highlighting Enabled

set mouse=a                 " Mouse support

" https://github.com/numirias/security
set nomodeline


set mousehide
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar

set noswapfile              " No swap files

" Setup Backup Drives
" if !has("nvim")
"     " Configure temp directories
"     if has("win32") || has("win32unix")
"       let s:tmpdir = expand("~/AppData/Local/Temp/vim")
"     else
"       let s:tmpdir = !empty($TMPDIR) ? $TMPDIR : "~/.tmp"
"       let s:tmpdir .= expand("/vim.$USER")
"     endif
"     :silent! call mkdir(s:tmpdir, "p", 0700)
"     let &dir = s:tmpdir
"     let &undodir = s:tmpdir
"     let &backupdir = s:tmpdir
" endif



set backupdir=~/.vim/backup/,~/.tmp      " backup files (~) in a common location if possible
set backup

" turn on undo files, put them in a common location
set undodir=~/.vim/undo/
set undofile
set undolevels=1000 "maximum number of changes that can be undone
set undoreload=10000 "maximum number lines to save for undo on a buffer reload

" Sets how many lines of history VIM has to remember
set history=200

" Set to auto read when a file is changed from the outside
set autoread

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = "\<Space>"
let g:mapleader = "\<Space>"



" Make sure airline is always showing
set laststatus=2

" Always show current position
set ruler

" Smart line numbers
autocmd FocusLost   * call LineNumberSet(1)
autocmd FocusGained * call LineNumberSet(0)
autocmd InsertEnter * call LineNumberSet(1)
autocmd InsertLeave * call LineNumberSet(0)

function! LineNumberSet(state)
  if(a:state == 1)
    set norelativenumber
    set number
    highlight LineNr ctermfg=yellow
  else
    set relativenumber
    highlight LineNr ctermfg=green
  endif
endfunction

noremap <leader>m :call LineNumberSet(&relativenumber)<cr>


" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
" " Allow backspacing over everything in insert mode.
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch

" How many tenths of a second to blink when matching brackets
set mat=2

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

set showcmd		" display incomplete commands

set wildmenu		" display completion matches in a status line

" Use Unix as the standard file type
set ffs=unix,dos,mac
" No annoying sound on errors
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=
set tm=500

filetype plugin indent on         " Enable file type detection.


"-----------cursor
let &t_SI.="\e[6 q" "SI = INSERT mode
let &t_SR.="\e[4 q" "SR = REPLACE mode
let &t_EI.="\e[2 q" "EI = NORMAL mode (ELSE)

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 2 spaces
set shiftwidth=2
set tabstop=2

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

" Support (24-bit) color

if exists('+termguicolors')
  let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Vim-plug
" Automatic installaion of vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !mkdir -p ~/.vim/autoload
  silent !curl -fLo ~/.vim/autoload/plug.vim
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
call plug#begin('~/.vim/plugged')

" fancy status lines
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'morhetz/gruvbox'
Plug 'ghifarit53/tokyonight-vim'
Plug 'altercation/vim-colors-solarized'
Plug 'skywind3000/asyncrun.vim'


Plug 'sheerun/vim-polyglot' " Syntax, colour schemes and the line

Plug 'jreybert/vimagit' " git status thing TODO(LEARN)
Plug 'tpope/vim-fugitive' " git wrapper TODO(LEARN)
Plug 'airblade/vim-gitgutter' " git diff stuff in the gutter TODO(Explore alternative vim-signify)

Plug 'tpope/vim-sleuth' " autodetect buffer settings
Plug 'tpope/vim-repeat' " make . (repeat) work for more plugins

Plug 'tpope/vim-endwise' " add end after if etc
Plug 'Raimondi/delimitMate'

Plug 'tpope/vim-unimpaired' " add complementary pairs of mappings [b and ]b are here! TODO(LEARN)
Plug 'tpope/vim-surround' " commands for surrounds TODO(LEARN, EXTEND)
Plug 'tpope/vim-commentary' " comment and uncomment lines TODO(LEARN)

" fzf searching, need to bind to ctrl-p
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'



Plug 'Yggdroot/indentLine' " show indent level
Plug 'terryma/vim-multiple-cursors' " sublime text style multiple cursors TODO(LEARN)
Plug 'Lokaltog/vim-easymotion' " god tier navigation stuff
Plug 'andymass/vim-matchup' " stuff for %. vim-surround, vim-sandwitch, are related

Plug 'christoomey/vim-tmux-navigator' " TODO(LEARN)
Plug 'godlygeek/tabular' " align text TODO(LEARN)

" Undo tree
Plug 'simnalamburt/vim-mundo'

" Project auto-cd
Plug 'airblade/vim-rooter'

"terminal
Plug 'iaalm/terminal-drawer.vim'

" Language
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'prabirshrestha/asyncomplete-ultisnips.vim'
" Plug 'hrsh7th/vim-vsnip'
" Plug 'hrsh7th/vim-vsnip-integ'
" COC
" Use release branch (recommended)
" Plug 'neoclide/coc.nvim', {'branch': 'release'}

" CMake
Plug 'cdelledonne/vim-cmake'
Plug 'NoahTheDuke/vim-just'


"Debugging
Plug 'puremourning/vimspector'

call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
" colorscheme gruvbox
let g:tokyonight_style = 'night' " available: night, storm
let g:tokyonight_enable_italic = 1

colorscheme tokyonight
set termguicolors
syntax enable

" Highlight eol whitespace
match Todo /\s\+$/


if has('win32') && !has('gui_running') && !empty($CONEMUBUILD)
  set term=xterm
  set t_Co=256
  let &t_AB="\e[48;5;%dm"
  let &t_AF="\e[38;5;%dm"
endif





"This unsets the "last search pattern" register by hitting return
nnoremap <cr> :noh<cr><cr>

" turn off ctrl-c in insert mode
inoremap <C-c> <Nop>


" Do not recognize octal numbers for Ctrl-A and Ctrl-X, most users find it
" confusing.
set nrformats-=octal " (default) vim 8+

" Don't use Ex mode, use Q for formatting.
" Revert with ":unmap Q".
map Q gq

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, splits and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" move vertically by visual line
nnoremap j gj
nnoremap k gk

" Smart way to move between splits
nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-h> <C-W>h
nmap <C-l> <C-W>l

" Close the current buffer
nmap <leader>q :Bclose<cr>

" Close current buffer and delete split
nmap <leader>qq :bd!<cr>

" Close all the buffers
nmap <leader>qa :%bd!<cr>

" Switch CWD to the directory of the open buffer
nmap <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab,newtab
  set stal=2
catch
endtry

" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
" Remember info about open buffers on close
set viminfo^=%

" edit vimrc/zshrc and load vimrc bindings
nnoremap <leader>ev :vsp ~/.vimrc<CR>
nnoremap <leader>ez :vsp ~/.zshrc<CR>
nnoremap <leader>sv :source ~/.vimrc<CR>

" exit terminal in nvim like normal
" <Esc> works for <C-[> too
tnoremap <Esc> <C-\><C-n>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Helper Functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Don't close window, when deleting a buffer
command! Bclose call <SID>BufcloseCloseIt()
function! <SID>BufcloseCloseIt()
   let l:currentBufNum = bufnr("%")
   let l:alternateBufNum = bufnr("#")

   if buflisted(l:alternateBufNum)
     buffer #
   else
     bnext
   endif

   if bufnr("%") == l:currentBufNum
     new
   endif

   if buflisted(l:currentBufNum)
     execute("bdelete! ".l:currentBufNum)
   endif
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use deoplete.
" let g:deoplete#enable_at_startup = 1
" let g:ale_completion_enabled = 1
" make syntastic not suck
" let g:syntastic_always_populate_loc_list = 1

" indentline char
let g:indentLine_char = '┊'
:set list lcs=tab:\┊\·

" Airline
let g:airline_powerline_fonts = 1
let g:airline_theme = "tokyonight"
let g:airline#extensions#tabline#enabled = 1
" let g:airline_theme                        = 'gruvbox'
let g:airline#extensions#branch#enabled    = 1
" let g:airline#extensions#syntastic#enabled = 1

" EasyMotion
let g:EasyMotion_smartcase         = 1
let g:EasyMotion_do_mapping        = 0
let g:EasyMotion_off_screen_search = 1

" autoclose
let g:AutoClosePreserveDotReg = 0

" stock vim bindings suck
" replace with easymotion ones
nmap f <Plug>(easymotion-s)
nmap s <Plug>(easymotion-s2)
nmap t <Plug>(easymotion-bd-t)
nmap <Leader>t <Plug>(easymotion-bd-t2)

" Gif config
nmap <Leader>l <Plug>(easymotion-lineforward)
nmap <Leader>j <Plug>(easymotion-j)
nmap <Leader>k <Plug>(easymotion-k)
nmap <Leader>h <Plug>(easymotion-linebackward)

let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion

" Mundo
nnoremap <leader>u :MundoToggle<cr>

" FZF
nnoremap <leader>p :Files<cr>

command! -bang -nargs=* Rgg call fzf#vim#grep("rg --column --line-number --no-heading --color=always --smart-case ".shellescape(<q-args>), 1, {'options': '--delimiter : --nth 4..'}, <bang>0)

nnoremap <leader>o :Rgg<cr>
nnoremap <leader>b :Buffers<cr>

" -- rustfmt
let g:rustfmt_autosave = 1
let g:rust_clip_command = 'pbcopy'


"--terminal drawer
" Change your shell. This is useful if the variable "shell" is used by too many plugins.
let g:terminal_drawer_shell = "bash"

" Change the binding to another one, if you need <C-t> for other thing.
let g:terminal_drawer_leader = "<C-`>"

" Tmux navigator 
let g:tmux_navigator_no_mappings = 1

noremap <silent> {Left-Mapping} :<C-U>TmuxNavigateLeft<cr>
noremap <silent> {Down-Mapping} :<C-U>TmuxNavigateDown<cr>
noremap <silent> {Up-Mapping} :<C-U>TmuxNavigateUp<cr>
noremap <silent> {Right-Mapping} :<C-U>TmuxNavigateRight<cr>
noremap <silent> {Previous-Mapping} :<C-U>TmuxNavigatePrevious<cr>




" -----------------------------
"  LSP
"  ----------------------------
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? asyncomplete#close_popup() . "\<cr>" : "\<cr>"


if executable('cmake-language-server')
  au User lsp_setup call lsp#register_server({
  \ 'name': 'cmake',
  \ 'cmd': {server_info->['cmake-language-server']},
  \ 'root_uri': {server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'build/'))},
  \ 'whitelist': ['cmake'],
  \ 'initialization_options': {
  \   'buildDirectory': 'build',
  \ }
  \})
endif

" Snippets
if has('python3')
    let g:UltiSnipsExpandTrigger="<c-e>"
    call asyncomplete#register_source(asyncomplete#sources#ultisnips#get_source_options({
        \ 'name': 'ultisnips',
        \ 'allowlist': ['*'],
        \ 'completor': function('asyncomplete#sources#ultisnips#completor'),
        \ }))
endif





function! CppCFileHeading()
  let s:line=line(".")
  call setline(s:line,"/*********************************************")
  call append(s:line,"* Description - ")
  call append(s:line+1,"* Author - Aviik Chakraborty")
  call append(s:line+2,"* Date - ".strftime("%b %d %Y"))
  call append(s:line+3,"* *******************************************/")
  unlet s:line
endfunction

imap <F4> <Esc>mz:execute FileHeading()<RET>`zjA

function! SplitWezterm()
  " Run the shell command to send the split-pane command to WezTerm CLI
  let cmd = "echo 'wezterm cli split-pane --bottom --percent 20'"
  silent! execute '!' . cmd
endfunction

" nnoremap <leader>w :call SplitWezterm()<CR>


