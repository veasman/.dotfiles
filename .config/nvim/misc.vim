" Plugin auto install / fix broken vim-plug
if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
    echo "Downloading junegunn/vim-plug to manage plugins..."
    silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
    silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
    autocmd VimEnter * PlugInstall
endif

" Remove white space
autocmd BufWritePre * %s/\s\+$//e
autocmd BufWritePre * %s/\n\+\%$//e
autocmd BufWritePre *.[ch] %s/\%$/\r/e

" Autocompile LaTeX
" For some weird reason you have to save twice for LaTeX to work properly
fun! SaveTwice()
    !pdflatex %
    !pdflatex %
endfun

autocmd BufWritePre *.tex :call SaveTwice()

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

" Lightbulb
autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
