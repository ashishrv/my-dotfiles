set nocompatible              " be iMproved, required
filetype off                  " required

" highlighting
set relativenumber
syntax on
set background=dark
set cursorline

" tabbing
set expandtab
set shiftwidth=2
set softtabstop=4

" folding settings
set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=1

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'airblade/vim-gitgutter'
Plugin 'kien/ctrlp.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" let g:solarized_termcolors=256

" get infinite undo in your files.
set undofile
set undodir=~/.vim/undodir

let s:undos = split(globpath(&undodir, '*'), "\n")
call filter(s:undos, 'getftime(v:val) < localtime() - (60 * 60 * 24 * 90)')
call map(s:undos, 'delete(v:val)')
