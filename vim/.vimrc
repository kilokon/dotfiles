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

set backupdir=~/.vim/backup/,~/tmp,.      " backup files (~) in a common location if possible
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

Plug 'vim-syntastic/syntastic'
" Plug 'scrooloose/synastic' " syntax checking

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

" COC
" Use release branch (recommended)
Plug 'neoclide/coc.nvim', {'branch': 'release'}

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


" make syntastic not suck
let g:syntastic_always_populate_loc_list = 1

" indentline char
let g:indentLine_char = '┊'
:set list lcs=tab:\┊\·

" Airline
let g:airline_powerline_fonts = 1
let g:airline_theme = "tokyonight"
let g:airline#extensions#tabline#enabled = 1
" let g:airline_theme                        = 'gruvbox'
let g:airline#extensions#branch#enabled    = 1
let g:airline#extensions#syntastic#enabled = 1

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




" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0 


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => COC configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if exists('&tagfunc')
  set tagfunc=CocTagFunc
endif


" May need for Vim (not Neovim) since coc.nvim calculates byte offset by count
" utf-8 byte sequence
" set encoding=utf-8
" Some servers have issues with backup files, see #649
" set nobackup
" set nowritebackup

" Having longer updatetime (default is 4000 ms = 4s) leads to noticeable
" delays and poor user experience
set updatetime=300

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate
" NOTE: There's always complete item selected by default, you may want to enable
" no select by `"suggest.noselect": true` in your configuration file
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config
inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion
if has('nvim')
" May need for Vim (not Neovim) since coc.nvim calculates byte offset by count
" utf-8 byte sequence
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s)
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying code actions to the selected code block
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying code actions at the cursor position
nmap <leader>ac  <Plug>(coc-codeaction-cursor)
" Remap keys for apply code actions affect whole buffer
nmap <leader>as  <Plug>(coc-codeaction-source)
" Apply the most preferred quickfix action to fix diagnostic on the current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Remap keys for applying refactor code actions
nmap <silent> <leader>re <Plug>(coc-codeaction-refactor)
xmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)
nmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)

" Run the Code Lens action on the current line
nmap <leader>cl  <Plug>(coc-codelens-action)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> to scroll float windows/popups
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges
" Requires 'textDocument/selectionRange' support of language server
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer
command! -nargs=0 Format :call CocActionAsync('format')

" Add `:Fold` command to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer
command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>






function! s:Highlight() abort
  if !has('gui_running') | hi normal guibg=NONE | endif
  call matchadd('ColorColumn', '\%81v', 100)
  hi ColorColumn ctermbg=magenta ctermfg=0 guibg=#333333
  hi HighlightedyankRegion term=bold ctermbg=0 guibg=#13354A
  hi CocCursorRange guibg=#b16286 guifg=#ebdbb2
  hi CursorLineNr  ctermfg=214 ctermbg=NONE guifg=#fabd2f guibg=NONE
  hi CocErrorFloat   guifg=#fb4934 guibg=#504945
  hi CocWarningFloat guifg=#fabd2f guibg=#504945
  hi CocInfoFloat    guifg=#d3869b guibg=#504945
  hi CocHintFloat    guifg=#83a598 guibg=#504945
  hi CocMenuSel      ctermbg=237 guibg=#504945
  hi link CocErrorSign    GruvboxRedSign
  hi link CocWarningSign  GruvboxYellowSign
  hi link CocInfoSign     GruvboxPurpleSign
  hi link CocHintSign     GruvboxBlueSign
  hi link CocFloating     Pmenu
  hi link MsgSeparator    MoreMsg
endfunction



augroup common
  autocmd!
  autocmd ColorScheme * call s:Highlight()
augroup end








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


