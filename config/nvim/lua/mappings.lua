--Vim Mappings

--Delete current buffer and replace it with new buffer
vim.keymap.set('n', '<leader>bd', ':bp<bar>sp<bar>bn<bar>bd<CR>')

--bind Alt-J and Alt+K to increment and decrement numbers
vim.keymap.set('n', '<A-k>','<C-a>')
vim.keymap.set('n', '<A-j>','<C-x>')

--use CTRL + hjkl in insert mode to move cursor (wont work for certain plugins, only insert mode.
--this is safe with tmux (probably thanks to tmux + neovim)
--vim.keymap.set('i', '<C-j>', '<Down>')
--vim.keymap.set('i', '<C-k>', '<Up>')
--vim.keymap.set('i', '<C-l>', '<Right>')
--vim.keymap.set('i', '<C-h>', '<Left>')

--When I do "d/", I'm intending to delete the selection included. I am usually NEVER trying to delete before the selection. This is just a mapping for my own sake.
vim.keymap.set('n', 'd/', function()
  vim.ui.input({
    prompt = "/",
    highlight = function(input)
      --TODO: still dont know how to do live highlighting yet, this doesn't seem to be called upon keypress?
      return {}
    end,
  }, function(input)
      --keypress explanation:
      --enter visual and search for the input. press enter and move back one character,
      --from there, delete the visual selection, then use gn to visually re-select the search input. then delete that.
      local keypresses = vim.api.nvim_replace_termcodes([[v/]]..input..[[<CR>hdgnd:noh<CR>]], true, true, true)
      vim.api.nvim_feedkeys(keypresses, 'n', false)
    end
  )
end);


