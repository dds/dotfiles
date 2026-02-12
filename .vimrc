" Minimal dark-mode .vimrc
" Auto-installs vim-plug and plugins on first run

" --- Bootstrap vim-plug ---
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" --- Plugins ---
call plug#begin('~/.vim/plugged')
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-unimpaired'
Plug 'nordtheme/vim', { 'as': 'nord' }
Plug 'morhetz/gruvbox'
Plug 'mbbill/undotree'
Plug 'jiangmiao/auto-pairs'
Plug 'sheerun/vim-polyglot'
call plug#end()

" --- General ---
filetype plugin indent on
syntax on
set hidden
set noswapfile
set undofile
set undodir=~/.vim/undodir
if !isdirectory(expand('~/.vim/undodir'))
  call mkdir(expand('~/.vim/undodir'), 'p')
endif

" --- Appearance ---
if $COLORTERM ==# 'truecolor' || $COLORTERM ==# '24bit'
  set termguicolors
endif
set background=dark
silent! colorscheme gruvbox
set number
set relativenumber
set signcolumn=yes
set laststatus=2
set showcmd
set scrolloff=8

" --- Statusline ---
set statusline=%f\ %m%r%h%w%=%y\ %l:%c\ %p%%

" --- Search ---
set incsearch
set hlsearch
set ignorecase
set smartcase

" --- Indentation ---
set autoindent
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4

" --- Behavior ---
set backspace=indent,eol,start
set mouse=a
set clipboard=unnamedplus
set splitbelow
set splitright
set wildmenu
set wildmode=longest:full,full
set updatetime=250
set ttimeoutlen=10

" --- Leader ---
let mapleader = ' '

" --- Keymaps ---
nnoremap <leader><space> :nohlsearch<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>

" Window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Move lines in visual mode
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Keep selection when indenting
vnoremap < <gv
vnoremap > >gv

" File explorer (netrw)
nnoremap <leader>n :Explore<CR>

" fzf
nnoremap <leader>p :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>g :Rg<CR>
nnoremap <leader>/ :BLines<CR>

" Fugitive
nnoremap <leader>gs :Git<CR>
nnoremap <leader>gd :Gdiffsplit<CR>
nnoremap <leader>gb :Git blame<CR>

" --- Strip trailing whitespace on save ---
autocmd BufWritePre * :%s/\s\+$//e
