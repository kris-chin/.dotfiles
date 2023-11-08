#!/usr/bin/env python3
import git
import argparse
from utils.git_utils import find_git_root
from utils.cli_utils import get_yes_or_no
from utils.general_utils import DictToObject
import os
import re
import logging

# This script stashes all changes and switches to a new branch
# If the new branch it switches to has stashed changes, it pops those changes from stash stack

#NOTE: this doesn't account for working on other peoples' branches, add handling for that
#TODO: detecting if this branch branched from main or another branch seems kinda weird. I skipped it for now but we should add functionality for that
#TODO: clean this code up, it be kinda messy

#Current Branch -> Target Branch
def main(script_args=None):
    #NOTE: for git feedback, logging MUST be enabled
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser()
    parser.add_argument("--dir", help="Directory which to run from", type=str)
    parser.add_argument("target_branch", help="Target Branch", type=str)
    args = DictToObject(**script_args) if (script_args != None) else parser.parse_args()

    directory = args.dir if (args.dir != None) else os.getcwd()
    print("Attempting to switch to: " + str(args.target_branch) + " from " + str(directory))

    repo = git.Repo(directory, search_parent_directories=True)

    current_branch = repo.active_branch
    target_branch = args.target_branch

    #1. If current Branch has unstaged changes, STASH the changes and name the stash the name of the branch
    hasUnstagedChanges = repo.is_dirty(untracked_files=True)
    if hasUnstagedChanges:
        #stash the changes and name the stash after the branch name
        #includes untracked
        print("Stashing all files")
        repo.git.stash("-m", "AUTO_STASH", "-u")

    #2. Switch to Target Branch
    target_branch = args.target_branch
    repo.git.checkout(args.target_branch)

    #3. Rebase with main (optional)
    if (get_yes_or_no("Fetch and Rebase with main?") == "y"):
        try:
            #switch to main real quick to update main
            repo.git.checkout("main")
            repo.git.fetch("--all")
            repo.git.pull("origin", "main")
            #then go back to original branch
            repo.git.checkout(args.target_branch)
            #now rebase
            repo.git.rebase("main")
        except Exception as e:
            print(f"ERROR: {e}")

    #4. If the target branch has an associated stash, pop it
    #get all stashes in the repo and convert each line into a list item
    stashes = list(repo.git.stash("list").splitlines())

    #this regex selects the stash number, the branch name, and the stash message
    stashRegex = r'stash@{(\d+)}: On ([^:]+): (.+)'
    for stash in stashes:
        match = re.search(stashRegex, stash)
        stash_number = match[1]
        stash_branch = match[2]
        stash_message = match[3]
        # if there is an existing stash, pop stash
        # if stash message is "AUTO_STASH" (reserved keyword for this system)
        if ((target_branch == stash_branch) and (stash_message == "AUTO_STASH")):
            print("Popping AUTO_STASH")
            repo.git.stash("pop", stash_number)

if __name__ == "__main__":
    main()
