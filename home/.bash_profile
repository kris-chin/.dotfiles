#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
[[ -f $HOME/.cargo/env ]] && . "$HOME/.cargo/env"

#add local bin to PATH (why is this not here already?)
export PATH=$PATH:$HOME/.local/bin
