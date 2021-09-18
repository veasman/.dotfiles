call plug#begin('~/.config/nvim/autoload/plugged')

" Colors
Plug 'gruvbox-community/gruvbox'
Plug 'joshdick/onedark.vim'
Plug 'ap/vim-css-color'

" Helpers
Plug 'sheerun/vim-polyglot'
"Plug 'vim-airline/vim-airline'
Plug 'mbbill/undotree'
Plug 'jiangmiao/auto-pairs'
Plug 'voldikss/vim-floaterm'
"Plug 'sbdchd/neoformat'
Plug 'mattn/emmet-vim'
Plug 'ThePrimeagen/vim-be-good'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-treesitter/playground'

" Intellisense
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
Plug 'kosayoda/nvim-lightbulb'

" Telescope
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'

call plug#end()
