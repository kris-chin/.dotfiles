# chin's Dotfiles

Just my dotfiles! Working on making this nicer and easier to bootstrap
TODO: OSX vs Arch support
TODO: Bootstrapping scripts

## Setup

- OSX Window Manager - Yabai + Skhd
- Terminal Emulator - Alacritty
- Shell - Zsh + Oh-My-Zsh / Bash
- Tmux
- Neovim
- Git
- Vim (just as legacy, not really for normal usage)
  - vim-plug

## Installed Software (wip)

- ack (search tool alternative to grep)
- brew 
- code-minimap (creates a minimap (for neovim))
- fd (faster "find" alternative)
- git-delta
- htop
- ispell (used for spelling on emacs)
- koekeishiya/formulae/skhd
- koekeishiya/formulae/yabai
- neofetch
- node
- nvm (used to manage nodejs versions)
- tmux
- ripgrep
- coreutils (helpful utilities)
- python3
- cloc (counts lines of code)
- ripgrep 
- docker (installed as cask)
- mysqlworkbench (installed as cask)
- emacs (installed as cask)
- angular-cli
- ruby
- rust (specifically for cargo)

### Globally-Installed Node Modules

- @mermaid-js/mermaid-cli@10.4.0 (mermaid JS) 

### Globally-Installed Python Packages

- GitPython
- jira
- requests
- urllib3

### Installed Arch Packages (WIP)

- picom
- feh
- xrandr

### Installed Ruby Gems

- colorls

### Installed MacOs Apps (WIP)

- ubersicht
- scroll reverser

### Installed Cargo Packages

- nerdfix

## All potential spots for colorschemes

- alacritty
    - `alacritty.yml`
    - I've been using `npx alacritty-themes` to easily manage a set number of themes, but I know there's more I can do
- neovim
    - `.config/nvim`
- zsh -> powerlevel10k prompts
    - `https://github.com/romkatv/powerlevel10k/blob/master/README.md#how-do-i-change-prompt-colors`
- tmux powerline
    - `.tmux.conf`
    - (I think there's also a way to configure it manaully as well, I don't know this one yet)
- ubersicht (macOS)
    - I don't know yet. 
- colorls  
    - `https://github.com/romkatv/powerlevel10k/blob/master/README.md#how-do-i-change-prompt-colors`
    - why the hell is this so complicated?
- vim
    - `.vimrc`
    - this is actually only used for the VERY few times vim is opened. (eg. git?) so no need to hyperfocus this being changed
- doom emacs
    - `.config/doom`
- yabai (macOS)
    - `.yabairc`
