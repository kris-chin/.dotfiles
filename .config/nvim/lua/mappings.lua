--Vim Mappings

--Delete current buffer and replace it with new buffer
vim.keymap.set('n', '<leader>bd', ':bp<bar>sp<bar>bn<bar>bd<CR>')

--use CTRL + hjkl in insert mode to move cursor (wont work for certain plugins, only insert mode.
--this is safe with tmux (probably thanks to tmux + neovim)
--vim.keymap.set('i', '<C-j>', '<Down>')
--vim.keymap.set('i', '<C-k>', '<Up>')
--vim.keymap.set('i', '<C-l>', '<Right>')
--vim.keymap.set('i', '<C-h>', '<Left>')
