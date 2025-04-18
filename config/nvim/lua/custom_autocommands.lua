--Remove the format options c,r, and o, which prevents autowrap and comment autoformatting 
vim.api.nvim_create_autocmd("BufEnter", {
  callback = function()
    vim.opt.formatoptions:remove { "c", "r", "o" }
  end,
  group = vim.api.nvim_create_augroup("General Settings", {clear = true}),
  desc = "Disable New Line Comment",
})

--HACK: For some reason, svelte's TreeSitter parser isn't enabling even when I have it set up to always be installed?
--This autocommand should enable treesitter highlighting EVERY time a buffer is displayed
function tsEnableHighlight ()
    vim.cmd[[:TSEnable highlight]]
end
vim.api.nvim_create_autocmd("BufWinEnter", {
  callback = function()
  --TODO: call only if treesitter is installed. this results in an error if it is not
    pcall(tsEnableHighlight)
  end,
  desc = "Always enable TreeSitter highlight on all files"
})
