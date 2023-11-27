#!/usr/bin/env python3

from utils.cli_utils import get_yes_or_no, get_user_choice, getExternalInput
from utils.jenkins_utils import setup_jenkins
from utils.tmux_utils import setup_tmux, find_first_zsh_pane
import sys
import os

def main1():
    print("Helllo. This is a test script from me experimenting with Python-Elisp integration.")
    y_n_choice = get_yes_or_no("Pick yes or no for this prompt.")
    if (y_n_choice == "y"):
        print("Unique yes choice")
    else:
        print("Unique no choice")

    some_options = ["option1", "option2", "option3"]
    choice = get_user_choice("Now pick an option", *some_options)
    print(f"You chose {choice}")
    print("Done! Will output to stdout:")
    sys.stdout.write("see ya!\n")

def main2():
    print("Hello this is testing external editor functionality")
    input("First, I'm going to ask for some input here to ensure that the output is being read line-by-line: ")
    print("We will now prompt for external input")
    externalInput = getExternalInput(os.path.expanduser("~/.config/doom/scripts/templates/pull_request.md"))
    print("externalinput:")
    print(externalInput)
    print("Yay!! we can then pass this to anything")

def main3():
    print("Hello this is testing Jenkins integrations")
    setup_jenkins()

def main4():
    print("Hello this is testing tmux integrations")
    server = setup_tmux()
    first_pane = find_first_zsh_pane(server)
    if (first_pane == None):
        print("No zsh pane found")
        return
    first_pane.send_keys("ls", enter=True)

if __name__ == "__main__":
    #main1()
    #main2()
    main4()
