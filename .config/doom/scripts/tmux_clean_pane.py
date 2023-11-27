#!/usr/bin/env python3

from utils.tmux_utils import setup_tmux, find_first_zsh_pane

#This script calls "clear" on the first zsh instance in the tmux server
def main():
    tmux = setup_tmux()
    first_pane = find_first_zsh_pane(tmux)
    if (first_pane == None):
        print("No ZSH pane found in existing tmux server!")

    first_pane.send_keys("clear", enter=True)

if __name__ == "__main__":
    main()
