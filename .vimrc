" Portable dark-mode .vimrc
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
Plug 'morhetz/gruvbox'
Plug 'preservim/nerdtree'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'vim-airline/vim-airline'
Plug 'dense-analysis/ale'
Plug 'airblade/vim-gitgutter'
call plug#end()

" --- General ---
set nocompatible
filetype plugin indent on
syntax on
set encoding=utf-8
set fileencoding=utf-8
set hidden
set nobackup
set nowritebackup
set noswapfile
set undofile
set undodir=~/.vim/undodir
if !isdirectory(expand('~/.vim/undodir'))
  call mkdir(expand('~/.vim/undodir'), 'p')
endif

" --- Appearance ---
set termguicolors
set background=dark
silent! colorscheme gruvbox
set number
set relativenumber
set cursorline
set signcolumn=yes
set laststatus=2
set noshowmode
set showcmd
set scrolloff=8
set sidescrolloff=8
set colorcolumn=80

" --- Search ---
set incsearch
set hlsearch
set ignorecase
set smartcase

" --- Indentation ---
set autoindent
set smartindent
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
set completeopt=menuone,noselect
set updatetime=250
set timeoutlen=500
set ttimeoutlen=10

" --- Leader ---
let mapleader = ' '

" --- Keymaps ---
" Clear search highlight
nnoremap <leader><space> :nohlsearch<CR>

" Quick save / quit
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>

" Better window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Resize splits
nnoremap <C-Up> :resize +2<CR>
nnoremap <C-Down> :resize -2<CR>
nnoremap <C-Left> :vertical resize -2<CR>
nnoremap <C-Right> :vertical resize +2<CR>

" Move lines up/down in visual mode
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Keep visual selection when indenting
vnoremap < <gv
vnoremap > >gv

" NERDTree
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <leader>f :NERDTreeFind<CR>
let NERDTreeShowHidden = 1
let NERDTreeMinimalUI = 1

" fzf
nnoremap <leader>p :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>g :Rg<CR>
nnoremap <leader>/ :BLines<CR>

" Fugitive
nnoremap <leader>gs :Git<CR>
nnoremap <leader>gd :Gdiffsplit<CR>
nnoremap <leader>gb :Git blame<CR>

" ALE
let g:ale_sign_error = 'E'
let g:ale_sign_warning = 'W'
let g:ale_fix_on_save = 1
nnoremap <leader>e :ALENext<CR>
nnoremap <leader>E :ALEPrevious<CR>

" Airline
let g:airline_powerline_fonts = 0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#ale#enabled = 1

" --- Strip trailing whitespace on save ---
autocmd BufWritePre * :%s/\s\+$//e
