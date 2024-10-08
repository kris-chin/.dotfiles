#Enable zsh profiling information
zmodload zsh/zprof

#Disable config wizard
POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

#
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  #TODO: uncomment this. for some reason. this causes zsh to crash when overriding manjaro's oh-my-zsh
  #source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#shell-side configs for emacs-vterm
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

#Point our oh-my-zsh custom folder instead to .config/zsh/custom
export ZSH_CUSTOM="$HOME/.config/zsh/custom"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes

#to future chin: if you wanna customize the prompt, do it via p10k
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
	zsh-vi-mode #custom plugin
	zsh-autosuggestions #custom plugin
	docker
	docker-compose
	dirhistory
	git
        virtualenv
	)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


#Set brew path
export PATH="/opt/homebrew/bin:$PATH"

#Add the ruby homebrew bin first to override the macOs default ruby
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"

# Add Cargo to our PATH
export PATH=$PATH:/home/krischin/.cargo/bin

#Add emacs to our PATH (for doom)
export PATH=$PATH:$HOME/.config/emacs/bin

#Set up rvm in zshrc, ONLY if it is installed
if [ -f /etc/profile.d/rvm.sh ]; then
  source /etc/profile.d/rvm.sh

  #Set up rvmsudo secure path
  export rvmsudo_secure_path=1

  export RVM_IS_INSTALLED=true
else 
  export RVM_IS_INSTALLED=false
fi

# Add ruby gems to our PATH
#TODO: in some situations, user_gemhome is undefined, not sure why..?
if command -v gem &>/dev/null; then
  export GEM_HOME="$(gem env user_gemhome)"
  export PATH="$PATH:$GEM_HOME/bin"
fi

#Run tmux and make sure it doesn't run itself
#I also added a variable that will disable tmux on startup. Just set it before running the terminal
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ] && [ -z "$NO_TMUX" ]; then
	exec tmux
fi

#If npm is installed, set the global node_modules
if command -v npm &> /dev/null; then
  export NODE_PATH=$(npm root --quiet -g)
fi

#speed up key sequences in zsh-vi-mode
export ZVM_KEYTIMEOUT=0.01

#configure the zsh-suggestions plugin to have a certain highlight text
#this is important for when your terminal emulator colorscheme's text colors conflict with this color and the suggestions dont show up
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

#C-hjkl in zsh insert mode (these don't work and i dont fucking know why!!!)
#TODO: wtf is a widget? (seems like a ZSH Concept)
#zvm_bindkey viins '^K' up-line-or-history zvm_readkeys_handler
#zvm_bindkey viins '^J' up-line-or-history zvm_readkeys_handler
#zvm_bindkey viins '^L' autosuggest-accept zvm_readkeys_handler

#colored LS
alias ls='ls --color=auto'

#use ack instead of grep
if command -v ack &>/dev/null; then
  alias grep='ack'
fi

#git stash aliases because I hate how long of an command it is
alias gsl='git stash list'
alias gsp='git stash pop'
alias gsm='git stash -m '

#also a git status alias becauase I use that a lot too
alias gs='git status'

#alias git checkout cuz that's too long for me
alias gch="git checkout"

#alias git rebase -i because i've been using it a lot lately
alias gri="git rebase -i "

#alias for quicker clears
alias c="clear"

#alias for switch-to-branch script
alias sb="python3 ~/.config/doom/scripts/git_switch_to_branch.py"

#export the jira cli token
#commented out because i'm using my PAT instead of the cli token
#export JIRA_API_TOKEN=$(cat ~/jira_cli_token)

#export the jira PAT (also for jira cli)
#TODO: uncomment once we create a temporary blank file
#export JIRA_API_TOKEN=$(cat ~/jira_token) 
export JIRA_AUTH_TYPE="bearer"

#show git dialog in gitpython
export GIT_PYTHON_TRACE="full"

#load personal values from a seperate file
#contains:
#PATH for emacs
#gsrefd
#gsrefb
#all variables for emacs FLOW
if [ -f ~/personal.env ]; then 
	source ~/personal.env
fi

#helper function that pushes the current branch to remote
function gpo () {
	local currentBranch=$(git rev-parse --abbrev-ref HEAD)
	git push origin $currentBranch "$@"
}

#helper function for creating git symbolic refs for better branch names 
function gsref () {
	local currentBranch=$(git rev-parse --abbrev-ref HEAD)
	git symbolic-ref refs/heads/$1 refs/heads/$currentBranch
}

#helper function that deletes branches on local that dont exist on remote anymore
function gdstale() {
  # Fetch the latest remote branch information
  git fetch

  # Get the list of local branches
	# BUG: this just gets the entire output and puts it into this variable
	local local_branches=()
	while IFS= read -r branch; do
    local_branches+=("$branch")
  done < <(git branch --format='%(refname:short)')

  # Iterate over each local branch
  for branch in $local_branches; do
		echo "Checking branch: $branch"
    # Check if the branch exists on the remote
		if ! git ls-remote --exit-code --quiet origin "$branch" >/dev/null 2>&1; then
      # Delete the local branch
      git branch -D "$branch"
    fi
  done
}

#if in wsl, set $DISPLAY to our VcXsrv server on windows
#NOTE: this is may need some configuring if you try this on a different system, since the local IP might change
if [[ $(is-wsl) == "True" ]]; then
  export DISPLAY=192.168.56.1:0.0
fi

#alias vim to mvim
#alias vim="mvim -v"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

#pacman alias because i'm lazy
alias pacman="sudo pacman"

#docker alias because I'm lazy
alias docker="sudo docker"
alias dockerd="sudo dockerd"
alias docker-compose="sudo docker-compose"

#R alias so we can install packages in a non-personal directory
alias R="sudo R"

# Load Angular CLI autocompletion for terminal. (only if ng exists)
if command -v ng &>/dev/null; then
  source <(ng completion script)
fi

# alias for vim to nvim because i will never change (ONLY if neovim is installed)
if command -v nvim &>/dev/null; then
  alias vim="nvim"
  alias v="nvim"
fi

# default git editor to vim
export GIT_EDITOR=vim

#setup tab completion for colorls, ONLY if it all exists
if command -v gem &>/dev/null && command -v colorls &>/dev/null; then
  source $(dirname $(gem which colorls))/tab_complete.sh
fi

#colorls aliass
#I have to add "rvmsudo" here because for some godforsaken reason, the way I have rvm set up makes it such that ruby does not have permission for... literally anything???
#that means that attempting to run colorls without rvmsudo yields an error because It seems that attempting to read a file without permission yields nil..
if [[ "$RVM_IS_INSTALLED" == true ]]; then
  alias lc='rvmsudo colorls -lA --sd'
  alias ls='rvmsudo colorls'
elif command -v colorls >/dev/null; then
  alias lc='colorls -lA --sd'
  alias ls='colorls'
fi

#source work-related aliases
#TODO: uncomment this once we can create a blank version of this file on startup
#source ~/scripts/aliases

#add local bin to PATH (why is this not here already?)
export PATH=$PATH:$HOME/.local/bin

#time function to time zsh startup
timezsh() {
  shell=${1-$SHELL}
  for i in $(seq 1 10); do time $shell -i -c exit; done
}

#logout alias 
alias logout="pkill X"

#alias to open emacs client ONLY if the daemon is running
function run_emacs() {
  if emacsclient -a false -e 't' &>/dev/null; then
    #First look at args provided and check to see which ones are flags
    local flag_count=0
    local nw_flag_provided=false

    for arg in "$@"; do
      #if the arg starts with a "-" (aka it is a flag)
      if [[ "$arg" == -* ]]; then
        flag_count=$((flag_count + 1))
        if [[ "$arg" == "-nw" ]]; then
          nw_flag_provided=true
        fi
      fi
    done

    #If -nw was not provided and no other flags were provided, pass the other args
    if [[ "$nw_flag_provided" == false && "$flag_count" -eq 0 ]]; then
      echo "Detected emacs server. Creating frame."

      #Open an emacsclient frame instead of a new emacs process 
      #for context, we should have the emacs daemon already running. this will just connect to the existing daemon
      #(INSANELY FASTER)
      #Run our emacs alias
      emacsclient --no-wait --create-frame $@
    #If -nw was provided and no other flags were provided, pass the other args
    elif [[ "$nw_flag_provided" == true && "$flag_count" -eq 1 ]]; then
      echo "Detected emacs server. Creating frame with '-nw'."
      emacsclient -nw --create-frame $@
    else 
      echo "Detected emacs server, but other flags were provided. Running 'emacs' with args"
      emacs $@
    fi
  else
    echo "No emacs server detected. Running regularly"

    #Run emacs with args
    emacs $@
  fi
}

#Kills emacs daemon
function kill_emacs() {
  emacsclient -e "(kill-emacs)"
}


#even more emacs aliases (TAKE THAT VIM!)
alias emacs="run_emacs"
alias e="run_emacs"
alias enw="run_emacs -nw"

#prompt tracking for emacs-vterm
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

#message-passing for emacs-vterm
vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}
