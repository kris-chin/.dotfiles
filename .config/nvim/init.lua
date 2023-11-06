vim.loader.enable()

--Runs the commands to load the old vimrc in vimscript
--vim.cmd([[
--	set runtimepath^=~/.vim runtimepath+=~/.vim/after
--	let &packpath=&runtimepath
--	source ~/.vimrc
--]])

local conf_files = {
    "globals.lua",
    "set.lua",
    "custom_autocommands.lua",
    "mappings.lua",
    "plugins.lua",
    "colorschemes.lua"
}

-- source all the core config files
for _, file_name in ipairs(conf_files) do
  if vim.endswith(file_name, 'vim') then
    local path = string.format("%s/core/%s", vim.fn.stdpath("config"), file_name)
    local source_cmd = "source " .. path
    vim.cmd(source_cmd)
  else
    local module_name, _ = string.gsub(file_name, "%.lua", "")
    package.loaded[module_name] = nil
    require(module_name)
  end
end
