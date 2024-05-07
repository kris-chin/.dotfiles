local lush = require("lush")
local hsl = lush.hsl

local M = {}

--local utils = require("colorschemes.wpg.lua.utils")
--local c = utils.c -- helper shorthand function to convert a hex string into rgb -> hsv and apply another hsv value to it
--local b = utils.b -- helper shorthand function to blend two hex strings

-- Read colors from colors.lua
local wpg =  require("colorschemes.wpg.base16")

--Create our colors
local black = hsl(wpg.black)
local red = hsl(wpg.red)
local blue = hsl(wpg.blue)
local yellow = hsl(wpg.yellow)
local green = hsl(wpg.green)
local magenta = hsl(wpg.magenta)
local cyan = hsl(wpg.cyan)
local white = hsl(wpg.bright_white)
local orange = hsl(wpg.red).hue(20).saturation(60).lighten(50)

-- disable LSP for this
---@diagnostic disable: undefined-global
return lush(function()
  return {
    -- big groups
    Normal      { fg = white, bg = black.lighten(5) },
    NormalFloat { fg = white, bg = black.lighten(5) },
    Comment     { fg = blue },
    Constant    { fg = cyan },
    String      { fg = yellow },
    Character   { fg = green },
    Number      { fg = orange },
    Boolean     { fg = orange },
    Float       { fg = orange },
    FloatBorder { fg = orange },
    Operator    { fg = orange },
    Statement   { fg = yellow },
    Type        { fg = green },
    TabLine     { fg = green },
    SignColumn  { fg = green },
    FoldColumn  { fg = green },
  }
end)
