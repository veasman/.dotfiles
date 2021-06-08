set exrc
set guicursor=
set relativenumber
set nohlsearch
set hidden
set noerrorbells
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set nu
set nowrap
set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile
set incsearch
set termguicolors
set scrolloff=8
set noshowmode
set completeopt=menuone,noinsert,noselect
set signcolumn=yes
"set t_Co=256

" Give more space for displaying messages.
set cmdheight=1

" Having longer updatetime (default is 4000ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=50

" Don't pass messages to |inns-completion-menu|.
set shortmess+=c

" Don't exceed 80 char long lines of code.
set colorcolumn=80

colorscheme gruvbox
"colorscheme onedark

highlight Normal guibg=none

" General workflow remaps
let mapleader = " "
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '>-2<CR>gv=gv
