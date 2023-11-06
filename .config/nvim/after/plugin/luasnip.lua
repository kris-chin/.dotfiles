local ls = require("luasnip")

--luasnip abbreviations
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local events = require("luasnip.util.events")
local ai = require("luasnip.nodes.absolute_indexer")
local extras = require("luasnip.extras")
local l = extras.lambda
local rep = extras.rep
local p = extras.partial
local m = extras.match
local n = extras.nonempty
local dl = extras.dynamic_lambda
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local conds = require("luasnip.extras.expand_conditions")
local postfix = require("luasnip.extras.postfix").postfix
local types = require("luasnip.util.types")
local parse = require("luasnip.util.parser").parse_snippet
local ms = ls.multi_snippet
local k = require("luasnip.nodes.key_indexer").new_key

--details about luasnip config:
--https://github.com/L3MON4D3/LuaSnip/blob/master/DOC.md#config-reference
--examples: https://github.com/L3MON4D3/LuaSnip/blob/master/Examples/snippets.lua

ls.setup({
    history = true,
    update_events = "TextChanged,TextChangedI",
    -- Snippets aren't automatically removed if their text is deleted.
	-- `delete_check_events` determines on which events (:h events) a check for
	-- deleted snippets is performed.
	-- This can be especially useful when `history` is enabled.
	delete_check_events = "TextChanged",
})

--add lua snippets here (more functionality)
--TO FUTURE CHIN: the luasnip equivilant to $1/$2 etc is to literally pass a function that steals the value from another node. (bruh)
--I think i'll need to make my own seperate folder for all of these cuz if im reusing code, that's so annoying
ls.add_snippets("all", {
    --easy index.js export
    --TODO: detect camelCase to kebab case
    s("i", {
	t("export * as "), i(1), t(" from \"./"), f(function(args, parent, user_args) local lowercase = string.lower(args[1][1]) return lowercase end, {1}), t("/index.js\";")
    })
})

--you can put vscode-style snippets here (less functionality)
require("luasnip.loaders.from_vscode").lazy_load({paths="~/.config/nvim/custom_snippets/vscode_snippets"})

--snipmate style snippets here: (these are cool!!)
require("luasnip.loaders.from_snipmate").lazy_load({paths = "~/.config/nvim/custom_snippets/snipmate_snippets"})

--a snippet list that shows currently available snippets
local sl = require("luasnip.extras.snippet_list")

--special note: you can pass args into sl-open to print it in a way that suits u
vim.keymap.set('n', '<leader>s', sl.open)
