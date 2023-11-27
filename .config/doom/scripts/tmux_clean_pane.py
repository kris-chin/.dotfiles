#!/usr/bin/env python3

from utils.tmux_utils import setup_tmux, find_first_zsh_pane, ensure_empty_insert_mode

#This script calls "clear" on the first zsh instance in the tmux server
def main():
    tmux = setup_tmux()
    first_pane = find_first_zsh_pane(tmux)
    if (first_pane == None):
        print("No ZSH pane found in existing tmux server!")
    ensure_empty_insert_mode(first_pane)
    first_pane.send_keys("clear", enter=True)

if __name__ == "__main__":
    main()
