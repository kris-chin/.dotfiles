"NOTE: This VIMRC is no longer updated in favor of NeoVim. Do not bother to update this unless necessary

"This loads vim-plug manager:
"By the way: also remember to call :PlugInstall afterwards
call plug#begin()

"Themes
"Plug 'morhetz/gruvbox'
Plug 'franbach/miramare'
"Plug 'dracula/vim', { 'as': 'dracula' }
"Plug 'srcery-colors/srcery-vim'
"Plug 'sainnhe/everforest'

"Syntax highlighting for a bunch of languages
Plug 'sheerun/vim-polyglot'

"Typescript Plugins
Plug 'pangloss/vim-javascript'

Plug 'HerringtonDarkholme/yats.vim'
Plug 'leafgarland/typescript-vim'

Plug 'maxmellon/vim-jsx-pretty'

"Tree for vim
Plug 'preservim/nerdtree'

"Nerdtree colors
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

"Icons for Vim
Plug 'ryanoasis/vim-devicons'

"Easymotion
Plug 'easymotion/vim-easymotion'

"VSCode-like completion
Plug 'neoclide/coc.nvim', {'branch' : 'release'}

"Fuzzy Finder Plugin for easier file finding
Plug 'ctrlpvim/ctrlp.vim'

"Vim Airline
Plug 'vim-airline/vim-airline'

"Vim Airline Themes
Plug 'vim-airline/vim-airline-themes'

"Vim Minimap (like vscode)
"Requires installing another package
Plug 'wfxr/minimap.vim'

"Vim Surround
Plug 'tpope/vim-surround'

"Fugitive (Proper Git integration with Vim wtihout needing to run bash
"commands in vim
Plug 'tpope/vim-fugitive'

"prettier for vim
"initializes with only supported files 
Plug 'prettier/vim-prettier', { 'do': 'npm install --frozen-lockfile --production' }

"view Git Diffs in the editor
Plug 'airblade/vim-gitgutter'

"This ends vim-plug
call plug#end()

"YATs needs this to prevent performance issues (and crashing)
set re=0

"Set background (sometimes needed for themes)
"set background=dark

"Set encoding to support icons
set encoding=UTF-8

"Syntax highlighting
syntax enable

"Terminal colors
" NOTE: some themes only work depending on terminal client / vim type. for macos, I
" needed mvim for the colors to work properly with termguicolors. this was
" despite the fact that iterm2 had support for trueterm
set termguicolors "(enables HEX colors for compatible terminals)

"italics support
let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"

"Theme 
colorscheme miramare

"Airline Theme
let g:airline_theme='dark_minimal'

"CoC extensions (Enables Language-Specific Code Completion
let g:coc_global_extensions = ['coc-tsserver']

"CoC Gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

"ALWAYS set the CWD to the currently selected file (useful for NERDtree)
"autocmd BufEnter * lcd %:p:h

"EasyMotion shortcut
"I've mapped m and n to bidirectional easymotion to help out with my muscle
"memory
map <Space>m <Plug>(easymotion-s)
map <Space>n <Plug>(easymotion-s)
vmap <Space>m <Plug>(easymotion-s)
vmap <Space>n <Plug>(easymotion-s)

"EasyMotion smartcase (lowercase matches lower + upper while upper only
"matches upper)
let g:EasyMotion_smartcase = 1

"Easymotion colors
hi EasyMotionTarget ctermbg=none ctermfg=red
hi EasyMotionTarget2First ctermbg=none ctermfg=red
hi EasyMotionTarget2Second ctermbg=none ctermfg=yellow

"Easy pane change
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <c-w>k
map <c-l> <c-w>l

"Set line numbers
set number relativenumber
"Color relative line numbers
hi LineNrAbove guifg=red ctermfg=red
hi LineNrBelow guifg=green ctermfg=green

"Set / and ? highlighting
set hlsearch

"Nerdtree config
nnoremap <leader>t :NERDTreeToggle<CR>
nnoremap <leader><leader>t :NERDTreeFind<CR>

"NerdTree window width
let g:NERDTreeWinSize = 60

"Closes NerdTree on file open
let g:NERDTreeQuitOnOpen = 1

"NerdTree syntax colors
let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1

"NerdTree LineNumbers
let NERDTreeShowLineNumbers = 1
autocmd FileType nerdtree setlocal relativenumber

"Start NERDTree when Vim is started without file arguments.
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists('s:std_in') | NERDTree | endif

"CtrlP set working directory
let g:ctrlp_working_path_mode = 'ra'

"CtrlP Ignore files in .gitignore
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

"CtrlP optimizations
"(moves cache and uses ag for search instead of vim's globpath() command
let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
if executable('ag')
	  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

"CtrlP easy buffer search mapping
map ,b :CtrlPBuffer<CR>

"Delete current buffer and replace it with next open buffer
map <leader>q :bp<bar>sp<bar>bn<bar>bd<CR>

"Convenience mapping for changing the current working directory
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>

"Convenience mapping to change CWD and then open tree 
map <leader><leader>cd <leader>cd<leader><leader>t

"Vim Minimap
let g:minimap_width = 10
let g:minimap_auto_start = 0
let g:minimap_auto_start_win_enter = 1

"Vim Minimap Toggle
nnoremap <leader>m :MinimapToggle<CR>

"vim settings to display diffs with gitgutter
set updatetime=100
set signcolumn=yes
