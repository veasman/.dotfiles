" Remove white space
autocmd BufWritePre * %s/\s\+$//e
autocmd BufWritePre * %s/\n\+\%$//e
autocmd BufWritePre *.[ch] %s/\%$/\r/e

" Remove dumb auto commenting new lines after a comment.
" This feature is a burden, I do not like it.
set comments=
set noai nocin nosi inde=
au FileType c,cpp setlocal comments-=:// comments+=f://
setlocal comments-=:// comments+=f://
set comments-=:// comments+=f://

" Set tab spacing for HTML files
autocmd Filetype html setlocal ts=2 sw=2 expandtab

" Auto run xrdb on Xresources save
autocmd BufWritePre .Xresources :!xrdb %

" Y acts like other capitals
nnoremap Y y$

" Keep cursor centered
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap J mzJ'z

" Undo break points
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ! !<c-g>u
inoremap ? ?<c-g>u

" Jumplist mutations
nnoremap <expr> k (v:count > 5 ? "m'" . v:count : "") . 'k'
nnoremap <expr> j (v:count > 5 ? "m'" . v:count : "") . 'j'

" Moving text around
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv
inoremap <C-j> <esc>:m .+1<CR>==
inoremap <C-k> <esc>:m .-2<CR>==
nnoremap <leader>j :m .+1<CR>==
nnoremap <leader>k :m .-2<CR>==
