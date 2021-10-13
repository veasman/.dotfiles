nnoremap <leader>ha :lua require('harpoon.mark').add_file()<CR>
nnoremap <leader>hc :lua require('harpoon.mark').clear_all()<CR>
nnoremap <leader>ht :lua require('harpoon.ui').toggle_quick_menu()<CR>
nnoremap <leader>a :lua require('harpoon.ui').nav_file(1)<CR>
nnoremap <leader>s :lua require('harpoon.ui').nav_file(2)<CR>
nnoremap <leader>d :lua require('harpoon.ui').nav_file(3)<CR>
nnoremap <leader>f :lua require('harpoon.ui').nav_file(4)<CR>
nnoremap <leader>t :lua require('harpoon.term').gotoTerminal(1)<CR>
