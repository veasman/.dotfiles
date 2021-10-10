" Telescope
nnoremap <leader>fi :lua require('telescope.builtin').find_files()<CR>
nnoremap <leader>fo :lua require('telescope.builtin').live_grep()<CR>
nnoremap <leader>fs :lua require('telescope.builtin').grep_string()<CR>
nnoremap <leader>vh :lua require('telescope.builtin').help_tags()<CR>
nnoremap <leader>vb :lua require('telescope.builtin').buffers()<CR>
