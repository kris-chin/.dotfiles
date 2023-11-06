--called when the plugin attaches. use this to set keymaps
local function my_on_attach(bufnr)
    local api = require('nvim-tree.api')
    local function opts(desc)
      return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
    end

    -- copy default mappings here from defaults in help section (see :h nvim-tree-mappings-default):
    vim.keymap.set('n', 'a',     api.fs.create,                         opts('Create'))
    vim.keymap.set('n', '<C-r>', api.fs.rename_sub,                     opts('Rename: Omit Filename'))
    vim.keymap.set('n', 'P',     api.node.navigate.parent,              opts('Parent Directory'))
    --SUPER USEFUL!!!
    vim.keymap.set('n', 'x',     api.fs.cut,                            opts('Cut'))
    vim.keymap.set('n', 'c',     api.fs.copy.node,                      opts('Copy'))
    vim.keymap.set('n', 'p',     api.fs.paste,                          opts('Paste'))
    vim.keymap.set('n', 'R',     api.tree.reload,                       opts('Refresh'))

    --use other defaults
    api.config.mappings.default_on_attach(bufnr)
end

require('nvim-tree').setup({
    --actions = {
    --    open_file = {
    --        quit_on_open = true
    --    }
    --},
    update_focused_file = {
        enable = true,
        update_root = false,
        ignore_list = {},
    },
    view = {
        relativenumber = true,
        width = 50
    },
    on_attach = my_on_attach,
    modified = {
        enable = true
    },
    filters = {
        git_ignored = false
    }
})

vim.keymap.set("n", "<leader>t", ":NvimTreeToggle<CR>")
