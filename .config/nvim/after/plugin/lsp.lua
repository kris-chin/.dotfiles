local lsp = require("lsp-zero")

lsp.preset("recommended")

lsp.on_attach(function(client, bufnr)
  -- see :help lsp-zero-keybindings
  -- to learn the available actions
  lsp.default_keymaps({buffer = bufnr})
end)

--configuration for lua language server specifically
require('lspconfig').lua_ls.setup(
    --combine lsp-zero's Lua config with our own
    vim.tbl_deep_extend(
        "force",
        lsp.nvim_lua_ls(),
        {
            on_init = function(client)
                --disable semantic tokens for Lua because it sucks for some reason
                client.server_capabilities.semanticTokensProvider = nil
            end
        }
    )
)
--configuration for tsserver specifically
require('lspconfig').tsserver.setup(
    {
        on_init = function(client)
            --ALSO disable semantic tokens for tsserver because it also sucks???? 
            client.server_capabilities.semanticTokensProvider = nil
        end
    }
)

--Fix undefined global 'vim'
lsp.nvim_workspace()

--Disable showing diagnostics on the side of a line
vim.diagnostic.config({
  virtual_text = false
})

--Show diagnostics automatically in a popup hover window
--i like this better than lsp_lines.vim, but i wish I could use both at the same time
vim.o.updatetime = 250
vim.cmd([[
  autocmd CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})
]])

--autocomplete config
--note: the "cmp" module comes from nvim-cmp
local cmp = require('cmp')
local cmp_select_opts = {behavior = cmp.SelectBehavior.Select}

--integrate cmp with luasnip
local luasnip = require("luasnip")

cmp.setup({
  snippet = {
      expand = function(args)
          luasnip.lsp_expand(args.body)
      end
  },
  sources = {
    {name = 'nvim_lsp'},
    {name = 'luasnip'}
  },
   mapping = {
       --press enter to confirm mapping
       ['<CR>'] = function(fallback)
            --if luasnip.expand_or_jumpable() then
            --   luasnip.expand_or_jump()
            if cmp.visible() then
                cmp.mapping.confirm({select = false})()
            else
                fallback()
            end
       end,
       --use C-k and c-j for navigating autocomplete
       ['<C-k>'] = function(fallback)
            --if luasnip.jumpable(-1) then
            --    luasnip.jump(-1)
            if cmp.visible() then
                cmp.mapping.select_prev_item(cmp_select_opts)()
            else
                fallback()
            end
       end,
        ['<C-j>'] = function(fallback)
            if cmp.visible() then
                cmp.mapping.select_next_item(cmp_select_opts)()
            else
                fallback()
            end
       end
   }
})

lsp.setup()
