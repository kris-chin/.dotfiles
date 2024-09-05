--Vim Settings

--Line numbers and relative line numbers
vim.opt.nu = true
vim.opt.relativenumber = true

--set the cursor line so it's easier to see the current line
vim.opt.cursorline = true
--enable cursorcolumn highlighting as well
vim.opt.cursorcolumn = true

--Tabs
vim.opt.shiftwidth = 2 --note: this is listened to over prettier? prettier seems to not help with this
vim.opt.expandtab = true

vim.opt.smartindent = true

--Set height of the commands below airline to 1
vim.opt.cmdheight = 1

--Set scrolloff to a very high amount such that the cursor is forever focused in the middle
vim.opt.scrolloff = 999

--Set icons for diagnostics
--Tip: copy and paste these from a nerdfonts cheatsheet
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

--set fold mode to indent folding
vim.opt.foldmethod = "indent"
--auto open all folds
vim.opt.foldlevel = 99

--integrate with wsl clipboard (taken from  :h wsl-clipboard)
vim.cmd([[
  let g:clipboard = {
    \   'name': 'WslClipboard',
    \   'copy': {
    \      '+': 'clip.exe',
    \      '*': 'clip.exe',
    \    },
    \   'paste': {
    \      '+': 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
    \      '*': 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
    \   },
    \   'cache_enabled': 0,
    \ }
]])
