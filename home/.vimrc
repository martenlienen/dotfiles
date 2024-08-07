filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Status line
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Hide the now reduntant mode display (e.g. --INSERT--)
set noshowmode

" Always show status line
set laststatus=2


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let mapleader = " "

inoremap fd <Esc>

nnoremap , :

" Move through windows
noremap <C-h> :wincmd h<CR>
noremap <C-j> :wincmd j<CR>
noremap <C-k> :wincmd k<CR>
noremap <C-l> :wincmd l<CR>

" Resize windows
nnoremap <Up> :wincmd +<CR>
nnoremap <Down> :wincmd -<CR>
nnoremap <Left> :wincmd <<CR>
nnoremap <Right> :wincmd ><CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets how many lines of history VIM has to remember
set history=700

" Enable local .vimrcs
set exrc
set secure


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Make VIM use utf-8 characters
set encoding=utf8

" Show line numbers
set number

" Show relative line numbers in normal mode and absolute ones
" in insert mode
set relativenumber
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

" Highlight the current line
set cursorline

" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Turn on the WiLd menu
set wildmenu

" Height of the command bar
set cmdheight=1

" Configure backspace so it acts as it should act
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

" No annoying sound on errors
set noerrorbells
set novisualbell


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable

" Manually enable 256 colors in gnome-terminal
if $COLORTERM == 'gnome-terminal'
  set t_Co=256

  " Use the colorscheme's background color to fill empty space
  " instead of the terminal's bg color
  set t_ut=
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git etc. anyway...
set nobackup
set nowb
set noswapfile

" Reload files when changed from outside vim
set autoread

" Use unix line endings
set fileformats=unix,dos,mac

" Save files in utf-8
set fileencoding=utf-8


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text formatting/editing
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tabs
set expandtab
set smarttab

" 1 tab == 2 spaces
set shiftwidth=2
set tabstop=2

" Do not wrap lines
set nowrap
