--Lazy (package loader)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

--disable netrw very early for nvim-tree
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require("lazy").setup({
  --which-key.nvim - displays a popup with possible keybindings when you call :WhichKey
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
        vim.o.timeout = true
        vim.o.timeoutlen = 300
    end,
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    }
  },
  --telescope - a fuzzy finder for neovim that also has a file preview???
  {
    'nvim-telescope/telescope.nvim', tag = '0.1.2',
    dependencies = {'nvim-lua/plenary.nvim'},
  },
  --nvim-tree a good tree for neovim (also has previews!)
  'nvim-tree/nvim-tree.lua',
  'nvim-tree/nvim-web-devicons',
  --treesitter - sets up a tree for the file. good for syntax highlighting for better colors
  {
      'nvim-treesitter/nvim-treesitter',
      run = function()
          local ts_update = require('nvim-treesitter.install').update({with_sync = true})
          ts_update()
      end,
      ensure_installed = { "c", "lua", "vim", "vimdoc", "javascript", "typescript" },
      highlight = {
          enable = true
      }
  },
  --colorschemes
  --dark high contrast themes
  "justb3a/vim-smarties",
  "pineapplegiant/spaceduck",
  "jdsimcoe/hyper.vim",
  "cdaddr/gentooish.vim",
  { "dracula/vim", name = "dracula" },
  "drazil100/dusklight.vim",

  --rustic themes
  "scottymoon/vim-twilight",
  "CreaturePhil/vim-handmade-hero",
  "hexared/Hypsteria",
  "sainnhe/gruvbox-material",

  --light themes
  "sainnhe/everforest",

  --lsp-zero, combines autocomplete and LSP. basically intellisense
  {
      "VonHeikemen/lsp-zero.nvim",
      branch = 'v2.x',
      dependencies = {
          --LSP config
          "neovim/nvim-lspconfig",
          --mason.nvim - a manager for ALL external editor stuff (like LSPs and linters)
          --call :Mason
          {
              "williamboman/mason.nvim",
              config = function()
                require("mason").setup()
              end
          },
          "williamboman/mason-lspconfig.nvim",

          --autocomplete
          {"hrsh7th/nvim-cmp"},
          {"hrsh7th/cmp-nvim-lsp"},
          {'L3MON4D3/LuaSnip'}
      }
  },
  "easymotion/vim-easymotion",
  --vim-fugitive - proper git integration
  {
      "tpope/vim-fugitive",
      config = function()
          -- disabling these because I like neogit
          -- vim.keymap.set('n', '<leader>gg', ':Git<CR>')
          -- vim.keymap.set('n', '<leader>gd', ':Git diff<CR>')
          -- vim.keymap.set('n', '<leader>gb', ':Git blame<CR>')
          -- vim.keymap.set('n', '<leader>gm', ':Git mergetool<CR>')
      end
  },
  --nvim surround
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
        require("nvim-surround").setup({
            -- Configuration here, or leave empty to use defaults
        })
    end
  },
  --peekup: lets you view contents in registers
  'gennaro-tedesco/nvim-peekup',
  --prettier for vim (adapted to neovim)
  --{
  --    'prettier/vim-prettier',
  --    build = "npm install --frozen-lockfile --production",
  --    config = function()
  --        vim.cmd([[
  --            let g:prettier#autoformat = 1
  --            let g:prettier#autoformat_require_pragma = 0
  --        ]])
  --    end
  --},
  --enables integration between nvim and tmux navigation (finally! have the same keys binds to navigate)
  {
      "alexghergh/nvim-tmux-navigation",
      config = function()
        local nvim_tmux_nav = require('nvim-tmux-navigation')
        --Tmux + NeoVim keybinds
        vim.keymap.set('n', '<C-h>', nvim_tmux_nav.NvimTmuxNavigateLeft)
        vim.keymap.set('n', '<C-j>', nvim_tmux_nav.NvimTmuxNavigateDown)
        vim.keymap.set('n', '<C-k>', nvim_tmux_nav.NvimTmuxNavigateUp)
        vim.keymap.set('n', '<C-l>', nvim_tmux_nav.NvimTmuxNavigateRight)
      end
  },
  --Easy convenience plugin to toggle transparent backgrounds
  {
      "xiyaowong/transparent.nvim",
      config = function()
          --Also disable NvimTree transparency
          require('transparent').clear_prefix('NvimTree')

          --Set keymaps to enable and disable transparency
          vim.keymap.set('n', '<leader>ct', ':TransparentToggle<CR>')
          --default to OFF
          vim.cmd([[:TransparentDisable]])
      end
  },
  --telescope filebrowser
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
  },
  --vim-sleuth: automatically adapts "shiftwidth" and "expandtab" based on file (automatic tab setting)
  --"tpope/vim-sleuth",
  --luasnippets: nvim snippet support
  {
    "L3MON4D3/LuaSnip",
    dependencies = { "rafamadriz/friendly-snippets" },
    -- follow latest release.
    version = "2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
    -- install jsregexp (optional!).
    build = "make install_jsregexp",
  },
  --integration with cmp and luasnip
  { 'saadparwaiz1/cmp_luasnip' },
  --lualine: a faster statusline (that's apparently better than airline... bruh)
  {
      'nvim-lualine/lualine.nvim',
      requires = { 'nvim-tree/nvim-web-devicons', opt = true }
  },
  --smooth scrolling on directional commands 
  --note: this seems to get laggy whenever I am not connected to a charger on iterm2 
  {
      "karb94/neoscroll.nvim",
      config = function()
          require('neoscroll').setup()
          local t = {}

          --configure the times for the scrolling
          -- Syntax: t[keys] = {function, {function arguments}}
          t['<C-u>'] = {'scroll', {'-vim.wo.scroll', 'true', '75', [['sine']]}} --default 250
          t['<C-d>'] = {'scroll', { 'vim.wo.scroll', 'true', '75', [['sine']]}} --default -250
          t['<C-b>'] = {'scroll', {'-vim.api.nvim_win_get_height(0)', 'true', '450'}} --default 450
          t['<C-f>'] = {'scroll', { 'vim.api.nvim_win_get_height(0)', 'true', '450'}} --default 450
          t['<C-y>'] = {'scroll', {'-0.10', 'false', '100'}} --default 100
          t['<C-e>'] = {'scroll', { '0.10', 'false', '100'}} --default 100
          t['zt']    = {'zt', {'250'}} --default 250
          t['zz']    = {'zz', {'250'}} --default 250
          t['zb']    = {'zb', {'250'}} --default 250

          require('neoscroll.config').set_mappings(t)
      end
  },
  --a scrollbar that shows diagnostics (very vscode-y)
  {
      "petertriho/nvim-scrollbar",
      config = function()
          require("scrollbar").setup()
      end
  },
  --show git diffs in the sign column
  "airblade/vim-gitgutter",
  --show more specific lines in diagnostics
  --disabled because I like the diagnostic float better, but one day I want to use both
  --{
  --    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
  --    config = function()
  --      require("lsp_lines").setup()
  --      --show virtual lines only for current line
  --      vim.diagnostic.config({ virtual_lines = { only_current_line = true } })
  --    end,
  --}
  --magit for neovim
  {
      "NeogitOrg/neogit",
      dependencies = {
        "nvim-lua/plenary.nvim",         -- required
        "nvim-telescope/telescope.nvim", -- optional
        "sindrets/diffview.nvim",        -- optional
        "ibhagwan/fzf-lua",              -- optional
      },
      config = function()
          local neogit = require("neogit")
          neogit.setup()
          vim.keymap.set('n', '<leader>g', function()
              neogit.open()
          end)

          return true
      end
    },
    --inline git blame
    "f-person/git-blame.nvim",
    --visual-multi : a multi-cursor mode similar to visual block
    {
        "mg979/vim-visual-multi",
        config = function()
            --there's apparently a VM_Maps variable that this plugin uses for keybindings as well
            --TODO: i'm trying to remap this to a more useful keybinding but I'm having a bad time trying to do that
            --see https://github.com/mg979/vim-visual-multi/issues/146
            vim.cmd([[
                let g:VM_maps = {}
                let g:VM_maps["Add Cursor Down"] = '<M-j>'
                let g:VM_maps["Add Cursor Up"] = '<M-k>'
            ]])
        end
    },
    --null-ls: support for neovim linting AND formatting (non-LSP programs) WITHIN neovim's LSP
    {
        "nvimtools/none-ls.nvim", --future note: this is compatible with null-ls, since null-ls was hard to maintain
        config = function()
            local null_ls = require("null-ls")
            local augroup = vim.api.nvim_create_augroup("LspFormatting", {}) --augroup for formatting

            null_ls.setup({
                sources = {
                    --put any sources you installed from mason here
                    --FUTURE NOTE: all of these sources have a ton of configuration options with the "with" method
                    null_ls.builtins.code_actions.eslint,
                    --special note regarding prettier: null-ls integrates with neovim, so neovim also can influence stuff like tabs
                    null_ls.builtins.formatting.prettier.with({
                        only_local = "node_modules/.bin", --force use the repo's prettier instead (nice) 
                    })
                },
                --the following on_attach function was just taken from the wiki, but this should effectively add auto-formatting on save
                -- you can reuse a shared lspconfig on_attach callback here
                on_attach = function(client, bufnr)
                    if client.supports_method("textDocument/formatting") then
                        vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
                        vim.api.nvim_create_autocmd("BufWritePre", {
                            group = augroup,
                            buffer = bufnr,
                            callback = function()
                                -- on 0.8, you should use vim.lsp.buf.format({ bufnr = bufnr }) instead
                                -- on later neovim version, you should use vim.lsp.buf.format({ async = false }) instead
                                --vim.lsp.buf.formatting_sync()
                                vim.lsp.buf.format({async = false})
                            end,
                        })
                    end
                end
            })
        end
    },
})
