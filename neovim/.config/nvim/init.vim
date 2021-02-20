if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'itchyny/lightline.vim'
" Plug 'morhetz/gruvbox'
Plug 'tpope/vim-surround'
" Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'w0rp/ale'
Plug 'airblade/vim-gitgutter'
Plug 'jiangmiao/auto-pairs'
" Plug 'osyo-manga/vim-anzu'
" Plug 'sheerun/vim-polyglot'
" Plug 'jceb/vim-orgmode'
Plug 'michaeljsmith/vim-indent-object'
Plug 'tommcdo/vim-lion'
Plug 'easymotion/vim-easymotion'
" Plug 'SirVer/ultisnips'

call plug#end()

" Plugin settings
filetype plugin indent on
let g:deoplete#enable_at_startup = 1
" let g:lightline = {'colorscheme' : 'gruvbox'}

" Defaults
set ignorecase
set smartcase
set laststatus=2
set relativenumber
set cursorline
set hlsearch
set backspace=2
set tabstop=4
set shiftwidth=4
set expandtab
set splitbelow
set splitright
set inccommand=nosplit
" set termguicolors

" Bindings
noremap <C-g> <C-c>
inoremap <C-g> <C-c>
imap <C-_> <Esc>
let mapleader=" "
nnoremap <leader>ff :Files <CR>
" nnoremap <leader>ff :Files <C-R>=expand('%:h')<CR><CR>
nnoremap <leader>fl :Locate <space>
nnoremap <leader>bb :Buffers <CR>
" Don't let easymotion get in the way of <leader><leader>
nmap <leader>j <Plug>(easymotion-prefix)
nnoremap <leader><leader> :Commands <CR>
nnoremap <leader>sr :Rg <space>

" nmap n <Plug>(anzu-n-with-echo)
" nmap N <Plug>(anzu-N-with-echo)
" nmap * <Plug>(anzu-star-with-echo)
" nmap # <Plug>(anzu-sharp-with-echo)
command DeleteTrailingWhitespace :%s/\s\+$//e
cnoreabbrev dtw DeleteTrailingWhitespace

set runtimepath^=/usr/share/vim/vimfiles

au VimLeave * set guicursor=a:ver1-blinkon0
