#
# ~/.bashrc
# I have a lot of lines of code here in case zsh is broken, but they really shouldnt be here
# TODO: Maybe simplify this file?
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

#Add cargo to PATH
export PATH=$PATH:~/.cargo/bin/

# Add ruby gems to our PATH
#TODO: uncomment once we figure out why user_gemhome is undefined
#export GEM_HOME="$(gem env user_gemhome)"
#export PATH="$PATH:$GEM_HOME/bin"

#Set git editor to vim
export GIT_EDITOR=vim
export VISUAL=vim
export EDITOR="$VISUAL"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

#add the current node's bin directory to path
if command -v nvm >/dev/null; then
  export NODE_PATH="$NVM_DIR/versions/node/$(nvm current)/bin"
  export PATH="$PATH:$NODE_PATH"
fi

if [ -f $HOME/.cargo/env ]; then
  . "$HOME/.cargo/env"
fi

if [ -f /etc/profile.d/rvm.sh ]; then
  source /etc/profile.d/rvm.sh
fi

#Start ZSH, if it exists
if command -v zsh >/dev/null; then
  zsh
  #Close bash if zsh gets exited
  exit
fi
