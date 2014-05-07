" funny I use emacs to edit this file...
colorscheme zenburn
" Path is automatically expanded to include QHOME content
set path=.,$QHOME

" Configuration below is inspired by: http://nvie.com/posts/how-i-boosted-my-vim/
set nocompatible
set hidden        " hide buffers instead of closing them
set nowrap        " don't wrap lines
set tabstop=4     " a tab is four spaces
set shiftwidth=4  " number of spaces to use for autoindenting
set smarttab      " insert tabs on the start of a line according to
                  "    shiftwidth, not tabstop
set backspace=indent,eol,start
                  " allow backspacing over everything in insert mode
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
set number        " always show line numbers
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set showmatch     " set show matching parenthesis
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase,
                  "    case-sensitive otherwise
set hlsearch      " highlight search terms
set incsearch     " show search matches as you type

set history=1000         " remember more commands and search history
set undolevels=1000      " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class
set title                " change the terminal's title
set visualbell           " don't beep
set noerrorbells         " don't beep

" filetype plugin indent on
" 
" if has('autocmd')
"    autocmd filetype python set expandtab
"    autocmd filetype k set expandtab
"    autocmd filetype q set expandtab
" endif

" highlight spaces and tabs in a more convenint way
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

" no arrow keys here, sorry
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" move around multiple windows
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" from http://forrst.com/posts/Use_w_to_sudo_write_a_file_with_Vim-uAN
cmap w!! w !sudo tee % >/dev/null
" Make yanking behave like kill-ring in emacs.
nnoremap <silent> <F11> :YRShow<CR>
" history folder is now ~/.vim instead of $HOME
let g:yankring_history_dir = "$HOME/.vim"
" format xml files using xmlling: http://ku1ik.com/2011/09/08/formatting-xml-in-vim-with-indent-command.html
au FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null
au FileType q setlocal et sw=2 ts=2 sts=2 cino+=}2
