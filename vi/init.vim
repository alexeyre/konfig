set langmap='z,\\,w,.e,pr,yt,fy,gu,ci,ro,lp,/[,=],aa,os,ed,uf,ig,dh,hj,tk,nl,sq,\\;q,-',qx,jc,kv,xb,bn,mm,w\\,,v.,z/,[-,#=,\"Z,<W,>E,PR,YT,FY,GU,CI,RO,LP,?{,+},AA,OS,ED,UF,IG,DH,HJ,TK,NL,SQ,_\",QX,JC,KV,XB,BN,MM,W<,V>,Z?,sq,SQ

call plug#begin()
        Plug 'sheerun/vim-polyglot'

        Plug 'lervag/vimtex'
        Plug 'ctrlpvim/ctrlp.vim'

        Plug 'NLKNguyen/papercolor-theme'
        Plug 'neovim/nvim-lspconfig'
        Plug 'williamboman/mason.nvim'
        Plug 'williamboman/mason-lspconfig.nvim'
call plug#end()

lua require("mason").setup()
lua require("mason-lspconfig").setup()

let g:markdown_folding = 1

hi htmlArg gui=italic
hi Comment gui=italic
hi Type    gui=italic
hi htmlArg cterm=italic
hi Comment cterm=italic
hi Type    cterm=italic


set background=dark
colorscheme PaperColor
let mapleader =","
set mouse=a
set clipboard+=unnamedplus
set nocompatible
filetype plugin on
syntax on
set encoding=utf-8
set number relativenumber
set splitbelow splitright

autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber 

filetype plugin indent on
let g:vimtex_view_general_viewer = 'open'
let g:vimtex_quickfix_ignore_filters = [
      \ 'Underfull',
      \ 'Overfull',
      \]

