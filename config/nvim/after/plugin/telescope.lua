local actions = require('telescope.actions')

require('telescope').setup({
    defaults = {
        -- Default configuration for telescope goes here:
        -- config_key = value,
        mappings = {
            i = {
                ['<C-j>'] = function(bufnr)
                    actions.move_selection_next(bufnr)
                end,
                ['<C-k>'] = function(bufnr)
                    actions.move_selection_previous(bufnr)
                end,
                ['<C-d>'] = function(bufnr)
                    actions.delete_buffer(bufnr)
                end
            }
        },
    },
    --pickers = {
    --    find_files = {
    --        theme = "dropdown"
    --    }
    --}
    extensions = {
      frecency = {
        show_scores = true --show ranking scores
      }
    }
})

--load file_browser extension for telescope
require("telescope").load_extension "file_browser"
local fb = require('telescope').extensions.file_browser

--telescope pickers
--regular find files
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<C-p>p', function()
    builtin.find_files({
        path_display={"truncate"},
        hidden=true,
        no_ignore=true
    })
end,{})
--live grep. requires ripgrep
vim.keymap.set('n', '<C-p>[', function()
    builtin.live_grep({
        path_display={"truncate"}
    })
end, {})

--buffers
vim.keymap.set('n', '<C-p>o', function()
    builtin.buffers({
        sort_mru=true,
        path_display={"truncate"}
    })
end, {})

--file browser for telescope
--opens file browser at current buffer
vim.keymap.set('n', '<C-p>0', function()
    fb.file_browser({
        path=vim.fn.expand("%:p:h"), --open at current file
        --TODO: change cwd as you navigate through the file browser, that way, when you toggle back to folder mode from file mode, you dont start back at the top
        grouped=true,
        select_buffer=true,
        hidden=false, --looking at all of the .git files is annoying
        initial_mode="insert",
        files=false --start in "folder browser" mode
    })
end, {noremap = true})
--useful keybind: C-f to toggle between folder and file

--set a keymap for the frecency extension
vim.keymap.set('n', '<C-p>;', "<Cmd>Telescope frecency workspace=CWD<CR>")

--set a keymap for highlights
vim.keymap.set('n', '<C-p>h', "<Cmd>Telescope highlights<CR>")
