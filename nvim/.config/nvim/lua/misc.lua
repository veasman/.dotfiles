vim.cmd([[
" Remove white space
autocmd BufWritePre * %s/\s\+$//e
autocmd BufWritePre * %s/\n\+\%$//e
autocmd BufWritePre *.[ch] %s/\%$/\r/e

" Set tab spacing for HTML files
autocmd Filetype html setlocal ts=2 sw=2 expandtab
autocmd Filetype ts setlocal ts=4 sw=4 expandtab
]])
