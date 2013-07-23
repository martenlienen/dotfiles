set nocompatible " be iMproved
filetype off     " required!

" Load vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
Bundle 'gmarik/vundle'

" My bundles
" Statusline
Bundle 'Lokaltog/powerline'

" Handling files
Bundle 'wincent/Command-T'
let g:CommandTAlwaysShowDotFiles = 1

" Rails
Bundle 'tpope/vim-rails.git'

" Coffeescript
Bundle 'kchmck/vim-coffee-script'

" HAML
Bundle 'tpope/vim-haml'

" Livescript
Bundle 'gkz/vim-ls'

" Clojure
Bundle 'tpope/vim-fireplace'
Bundle 'guns/vim-clojure-static'

filetype plugin indent on " required!

" Activate powerline
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim

" Enable special characters in powerline
let g:Powerline_symbols = 'fancy'

" Hide the now reduntant mode display (e.g. --INSERT--)
set noshowmode


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let mapleader = " "

inoremap jj <Esc>
inoremap <Esc> <Nop>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets how many lines of history VIM has to remember
set history=700


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Make VIM use utf-8 characters
set encoding=utf8

" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Turn on the WiLd menu
set wildmenu

" Ignore compiled files
set wildignore=*.o,*~,*.pyc

"Always show current position
set ruler

" Height of the command bar
set cmdheight=1

" A buffer becomes hidden when it is abandoned
set hid

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
" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set novisualbell


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable

colorscheme desert
set background=dark


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

set autoindent
set smartindent

" Do not wrap lines
set nowrap

