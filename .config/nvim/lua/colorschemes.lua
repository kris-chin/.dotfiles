-- Loads a random colorscheme on nvim startup 

local utils = require("utils")

local M = {}

--wrap all colorscheme calls with this function, which calls additional color changes after the colorscheme
--TODO: this probably would be better as an autocmd rather than a wrapper function. might not work with certain plugins
local function wrapColorScheme(colorSchemeFn)
    --first, call the colorscheme function
    colorSchemeFn()

    --next, let's set our relative line numbers
    --vim.cmd([[highlight LineNrAbove guifg=red]])
    --vim.cmd([[highlight LineNrBelow guifg=green]])

    --remove weird gray box for the sign column
    vim.cmd([[highlight clear SignColumn]])
end

--helper function that just is called at the bottom of the file
local function setupColorscheme()
    --enable termguicolors, only disable this to use the terminal emulator's colors
    --vim.opt.termguicolors = false
    vim.opt.termguicolors = true

    M.rand_colorscheme()
end

--- A mapping between the name of the theme and the repo name
-- string is the repo name, value is the color profile name
M.colorscheme2dir = {
    --dark, high contrast themes
    --smarties = "smarties-vim"
    --spaceduck = "spaceduck",
    --dracula = "dracula",
    --gentooish = "gentooish",
    --hyper = "hyper.vim",
    --dusklight = "dusklight-vim",

    --rustic themes
    --hypsteria = "hypsteria",
    ['gruvbox-material'] = "gruvbox-material"
    --['handmade-hero'] = "vim-handmade-hero",
    --twilight = "vim-twilight",

    --light themes
    --everforest = "everforest",

    --other themes
}

--- Use a random colorscheme from the pre-defined list of colorschemes.
M.rand_colorscheme = function()
  local colorscheme = utils.rand_element(vim.tbl_keys(M.colorscheme2dir))

  --Attempt to get a custom function for the colorscheme
  local mFunction = M[colorscheme]

  --if it doesnt exist, use a generic function
  if mFunction == nil then
      local colorSchemeCmd = "colorscheme " .. colorscheme
      mFunction = function()
          vim.cmd(colorSchemeCmd)
      end
  end

  --call the wrapping colorscheme function which adds additional colors
  wrapColorScheme(mFunction)
  local msg = "Colorscheme: " .. colorscheme
  vim.notify(msg)

end

M.everforest = function()
  vim.g.everforest_enable_italic = 1
  vim.g.everforest_better_performance = 1

  vim.cmd([[colorscheme everforest]])
end

M.spaceduck = function()
    --Italics support
    --if exists('+termguicolors')
    --  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    --  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    --  set termguicolors
    --endif
   vim.cmd([[colorscheme spaceduck]])
   --white comments because I hate the comment color
   vim.cmd([[highlight Comment ctermfg=white guifg=white]])
end

setupColorscheme()

--keybind to set a random colorscheme
vim.keymap.set('n', '<leader>c', M.rand_colorscheme)
