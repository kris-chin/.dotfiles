--Remove the format options c,r, and o, which prevents autowrap and comment autoformatting 
vim.api.nvim_create_autocmd("BufEnter", {
  callback = function()
    vim.opt.formatoptions:remove { "c", "r", "o" }
  end,
  group = vim.api.nvim_create_augroup("General Settings", {clear = true}),
  desc = "Disable New Line Comment",
})
