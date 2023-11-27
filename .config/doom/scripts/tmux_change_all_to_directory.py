#!/usr/bin/env python3
#this script takes EVERY zsh pane and calls "cd" on it to a given directory

import argparse
from utils.tmux_utils import setup_tmux, find_all_zsh_panes
from utils.general_utils import DictToObject

def GetArguments(script_args=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("dir", type=str, help="The directory to switch to")
    args = DictToObject(**script_args) if (script_args != None) else parser.parse_args()
    return args

def main(script_args=None):
    args = GetArguments(script_args)

    tmux = setup_tmux()
    zsh_panes = find_all_zsh_panes(tmux)
    if (len(zsh_panes) == 0):
        print("Could not find any zsh panes!")

    for pane in zsh_panes:
        print("Attempting pane: " + str(pane))
        pane.send_keys(f"cd {args.dir}", enter=True)

if __name__ == "__main__":
    main()
