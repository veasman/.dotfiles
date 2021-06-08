" Telescope
nnoremap <leader>fi :lua require('telescope.builtin').find_files()<CR>
nnoremap <leader>fs :lua require('telescope.builtin').grep_string()<CR>
nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
