--lowercase matches lower + upper while upper only matches upper
vim.g.EasyMotion_smartcase = 1

vim.keymap.set('n', '<Space>m', '<Plug>(easymotion-s)')
vim.keymap.set('n', '<Space>n', '<Plug>(easymotion-s)')
vim.keymap.set('v', '<Space>m', '<Plug>(easymotion-s)')
vim.keymap.set('v', '<Space>n', '<Plug>(easymotion-s)')
