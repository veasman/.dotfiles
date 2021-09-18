" Main
source $HOME/.config/nvim/plug.vim
source $HOME/.config/nvim/general.vim
source $HOME/.config/nvim/misc.vim

" Plugin config
source $HOME/.config/nvim/plugins/emmet.vim
source $HOME/.config/nvim/plugins/floaterm.vim
source $HOME/.config/nvim/plugins/telescope.vim
source $HOME/.config/nvim/plugins/undotree.vim
luafile $HOME/.config/nvim/lua/treesitter-config.lua

" Intellisense setup
source $HOME/.config/nvim/plugins/lsp-config.vim
luafile $HOME/.config/nvim/lua/compe-config.lua
luafile $HOME/.config/nvim/lua/lightbulb-config.lua

" Language servers
luafile $HOME/.config/nvim/lua/bashls-bash.lua
luafile $HOME/.config/nvim/lua/clangd-cpp.lua
luafile $HOME/.config/nvim/lua/cssls-css.lua
luafile $HOME/.config/nvim/lua/htmlls-html.lua
luafile $HOME/.config/nvim/lua/jsonls-json.lua
luafile $HOME/.config/nvim/lua/pylsp-python.lua
"luafile $HOME/.config/nvim/lua/rust-analyzer-rust.lua
luafile $HOME/.config/nvim/lua/sumneko-lua.lua
luafile $HOME/.config/nvim/lua/tsserver-typescript.lua
luafile $HOME/.config/nvim/lua/vimls-vim.lua
